/* Native Prometheus metrics for Emacs daemons.
   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#include "lisp.h"
#include "systhread.h"
#include "systime.h"

#include "zmetrics.h"

enum
  {
    ZMETRICS_STALL_SECONDS = 3,
    ZMETRICS_REQUEST_CAPACITY = 2048,
    ZMETRICS_RESPONSE_CAPACITY = 8192
  };

static uint64_t zmetrics_started_ns;
static uint64_t zmetrics_checkpoint_ns;
static uint64_t zmetrics_checkpoint_count;
static uint64_t zmetrics_observer_ns;
static uint64_t zmetrics_recursive_edit_depth;
static uint64_t zmetrics_minibuffer_depth;
static uint64_t zmetrics_lisp_eval_depth;
static uint64_t zmetrics_waiting_for_input;
static uint64_t zmetrics_nested_input_wait;
static uint64_t zmetrics_nested_input_wait_started_ns;
static intmax_t zmetrics_input_wait_eval_depth = INTMAX_MAX;
static uint64_t zmetrics_gc_started_ns;
static uint64_t zmetrics_gc_count;
static uint64_t zmetrics_gc_total_ns;
static uint64_t zmetrics_gc_last_ns;
static uint64_t zmetrics_gc_max_ns;
static uint64_t zmetrics_gc_active;
static uint64_t zmetrics_http_requests;

static uint64_t
zmetrics_now_ns (void)
{
  struct timespec now = monotonic_coarse_timespec ();
  return (uint64_t) now.tv_sec * 1000000000 + now.tv_nsec;
}

static uint64_t
zmetrics_load (uint64_t *value)
{
  return __atomic_load_n (value, __ATOMIC_RELAXED);
}

static void
zmetrics_store (uint64_t *value, uint64_t new_value)
{
  __atomic_store_n (value, new_value, __ATOMIC_RELAXED);
}

/* A periodic atimer is not a liveness probe: Emacs services native atimers
   while Lisp is stuck in such synchronous calls as `call-process'.  Observe
   transitions into the real input wait instead.  The minimum evaluation depth
   seen there is the outer command loop; a deeper input wait is a prompt (or
   equivalent read) entered from active Lisp and must not reset the stall age.  */
static void
zmetrics_observe_main_thread (bool input_wait_active,
			      bool reset_checkpoint)
{
  uint64_t before = zmetrics_now_ns ();
  intmax_t eval_depth = lisp_eval_depth;
  bool nested_input_wait = false;

  if (input_wait_active)
    {
      if (eval_depth < zmetrics_input_wait_eval_depth)
	zmetrics_input_wait_eval_depth = eval_depth;
      nested_input_wait
	= (eval_depth > zmetrics_input_wait_eval_depth
	   || command_loop_level > 0 || minibuf_level > 0);
    }

  if (reset_checkpoint && !nested_input_wait)
    {
      zmetrics_store (&zmetrics_checkpoint_ns, before);
      __atomic_fetch_add (&zmetrics_checkpoint_count, 1, __ATOMIC_RELAXED);
    }
  zmetrics_store (&zmetrics_recursive_edit_depth,
		  command_loop_level < 0 ? 0 : command_loop_level);
  zmetrics_store (&zmetrics_minibuffer_depth,
		  minibuf_level < 0 ? 0 : minibuf_level);
  zmetrics_store (&zmetrics_lisp_eval_depth,
		  eval_depth < 0 ? 0 : eval_depth);

  if (nested_input_wait)
    {
      if (!zmetrics_load (&zmetrics_nested_input_wait))
	zmetrics_store (&zmetrics_nested_input_wait_started_ns, before);
      zmetrics_store (&zmetrics_nested_input_wait, 1);
    }
  else if (input_wait_active)
    {
      zmetrics_store (&zmetrics_nested_input_wait, 0);
      zmetrics_store (&zmetrics_nested_input_wait_started_ns, 0);
    }

  uint64_t after = zmetrics_now_ns ();
  __atomic_fetch_add (&zmetrics_observer_ns, after - before,
		      __ATOMIC_RELAXED);
  zmetrics_store (&zmetrics_waiting_for_input,
		  input_wait_active && !nested_input_wait);
}

void
zmetrics_main_thread_wait_begin (void)
{
  zmetrics_observe_main_thread (true, true);
}

void
zmetrics_main_thread_wait_end (void)
{
  bool responsive_wait = zmetrics_load (&zmetrics_waiting_for_input);
  zmetrics_observe_main_thread (false, responsive_wait);
}

void
zmetrics_gc_begin (void)
{
  zmetrics_store (&zmetrics_gc_started_ns, zmetrics_now_ns ());
  zmetrics_store (&zmetrics_gc_active, 1);
}

void
zmetrics_gc_end (void)
{
  uint64_t now = zmetrics_now_ns ();
  uint64_t started = zmetrics_load (&zmetrics_gc_started_ns);
  uint64_t elapsed = started <= now ? now - started : 0;

  __atomic_fetch_add (&zmetrics_gc_count, 1, __ATOMIC_RELAXED);
  __atomic_fetch_add (&zmetrics_gc_total_ns, elapsed, __ATOMIC_RELAXED);
  zmetrics_store (&zmetrics_gc_last_ns, elapsed);

  uint64_t previous = zmetrics_load (&zmetrics_gc_max_ns);
  while (previous < elapsed
	 && !__atomic_compare_exchange_n (&zmetrics_gc_max_ns, &previous,
					 elapsed, false, __ATOMIC_RELAXED,
					 __ATOMIC_RELAXED))
    continue;

  zmetrics_store (&zmetrics_gc_active, 0);
}

struct zmetrics_buffer
{
  char *data;
  size_t capacity;
  size_t length;
};

static bool
zmetrics_append (struct zmetrics_buffer *buffer, char const *format, ...)
{
  if (buffer->length >= buffer->capacity)
    return false;

  va_list args;
  va_start (args, format);
  int count = vsnprintf (buffer->data + buffer->length,
			 buffer->capacity - buffer->length, format, args);
  va_end (args);

  if (count < 0 || (size_t) count >= buffer->capacity - buffer->length)
    {
      buffer->length = buffer->capacity;
      return false;
    }
  buffer->length += count;
  return true;
}

static bool
zmetrics_append_seconds (struct zmetrics_buffer *buffer, uint64_t ns)
{
  return zmetrics_append (buffer, "%llu.%09llu",
			  (unsigned long long) (ns / 1000000000),
			  (unsigned long long) (ns % 1000000000));
}

static bool
zmetrics_metric_seconds (struct zmetrics_buffer *buffer, char const *name,
			 uint64_t ns)
{
  return zmetrics_append (buffer, "%s ", name)
    && zmetrics_append_seconds (buffer, ns)
    && zmetrics_append (buffer, "\n");
}

static size_t
zmetrics_render_metrics (char *output, size_t capacity)
{
  struct zmetrics_buffer buffer = { output, capacity, 0 };
  uint64_t now = zmetrics_now_ns ();
  uint64_t checkpoint = zmetrics_load (&zmetrics_checkpoint_ns);
  bool input_wait_active = zmetrics_load (&zmetrics_waiting_for_input);
  uint64_t age
    = input_wait_active || checkpoint > now ? 0 : now - checkpoint;
  uint64_t nested_input_wait_started
    = zmetrics_load (&zmetrics_nested_input_wait_started_ns);
  uint64_t nested_input_wait_age
    = (nested_input_wait_started && nested_input_wait_started <= now
       ? now - nested_input_wait_started : 0);

#define ZMETRICS_TEXT(text) zmetrics_append (&buffer, text)
#define ZMETRICS_VALUE(name, value) \
  zmetrics_append (&buffer, name " %llu\n", (unsigned long long) (value))

  ZMETRICS_TEXT
    ("# HELP emacs_main_thread_checkpoint_age_seconds Seconds the Lisp/UI thread has spent away from its responsive input wait.\n"
     "# TYPE emacs_main_thread_checkpoint_age_seconds gauge\n");
  zmetrics_metric_seconds (&buffer, "emacs_main_thread_checkpoint_age_seconds",
			   age);
  ZMETRICS_TEXT
    ("# HELP emacs_main_thread_stalled Whether the Lisp/UI thread has missed checkpoints for at least three seconds.\n"
     "# TYPE emacs_main_thread_stalled gauge\n");
  ZMETRICS_VALUE ("emacs_main_thread_stalled",
		  age >= ZMETRICS_STALL_SECONDS * 1000000000ULL);
  ZMETRICS_TEXT
    ("# HELP emacs_main_thread_stall_threshold_seconds Checkpoint age that marks the Lisp/UI thread stalled.\n"
     "# TYPE emacs_main_thread_stall_threshold_seconds gauge\n"
     "emacs_main_thread_stall_threshold_seconds 3\n"
     "# HELP emacs_main_thread_checkpoints_total Responsive input-wait transitions observed on the Lisp/UI thread.\n"
     "# TYPE emacs_main_thread_checkpoints_total counter\n");
  ZMETRICS_VALUE ("emacs_main_thread_checkpoints_total",
		  zmetrics_load (&zmetrics_checkpoint_count));
  ZMETRICS_TEXT
    ("# HELP emacs_main_thread_observer_seconds_total Time spent by the Lisp/UI thread updating native observer atomics.\n"
     "# TYPE emacs_main_thread_observer_seconds_total counter\n");
  zmetrics_metric_seconds (&buffer, "emacs_main_thread_observer_seconds_total",
			   zmetrics_load (&zmetrics_observer_ns));
  ZMETRICS_TEXT
    ("# HELP emacs_main_thread_waiting_for_input Whether the Lisp/UI thread is in its responsive input wait.\n"
     "# TYPE emacs_main_thread_waiting_for_input gauge\n");
  ZMETRICS_VALUE ("emacs_main_thread_waiting_for_input",
		  input_wait_active);
  ZMETRICS_TEXT
    ("# HELP emacs_lisp_recursive_edit_depth Current recursive command-loop depth.\n"
     "# TYPE emacs_lisp_recursive_edit_depth gauge\n");
  ZMETRICS_VALUE ("emacs_lisp_recursive_edit_depth",
		  zmetrics_load (&zmetrics_recursive_edit_depth));
  ZMETRICS_TEXT
    ("# HELP emacs_lisp_minibuffer_depth Current active minibuffer depth.\n"
     "# TYPE emacs_lisp_minibuffer_depth gauge\n");
  ZMETRICS_VALUE ("emacs_lisp_minibuffer_depth",
		  zmetrics_load (&zmetrics_minibuffer_depth));
  ZMETRICS_TEXT
    ("# HELP emacs_lisp_eval_depth Lisp evaluation depth at the latest input-wait transition.\n"
     "# TYPE emacs_lisp_eval_depth gauge\n");
  ZMETRICS_VALUE ("emacs_lisp_eval_depth",
		  zmetrics_load (&zmetrics_lisp_eval_depth));
  ZMETRICS_TEXT
    ("# HELP emacs_lisp_nested_input_wait Whether Lisp entered a prompt or input wait from inside active evaluation.\n"
     "# TYPE emacs_lisp_nested_input_wait gauge\n");
  ZMETRICS_VALUE ("emacs_lisp_nested_input_wait",
		  zmetrics_load (&zmetrics_nested_input_wait));
  ZMETRICS_TEXT
    ("# HELP emacs_lisp_nested_input_wait_seconds Seconds spent in the current nested prompt or input wait.\n"
     "# TYPE emacs_lisp_nested_input_wait_seconds gauge\n");
  zmetrics_metric_seconds (&buffer, "emacs_lisp_nested_input_wait_seconds",
			   nested_input_wait_age);
  ZMETRICS_TEXT
    ("# HELP emacs_gc_in_progress Whether garbage collection is currently running.\n"
     "# TYPE emacs_gc_in_progress gauge\n");
  ZMETRICS_VALUE ("emacs_gc_in_progress",
		  zmetrics_load (&zmetrics_gc_active));
  ZMETRICS_TEXT
    ("# HELP emacs_gc_collections_total Garbage collections completed by this process.\n"
     "# TYPE emacs_gc_collections_total counter\n");
  ZMETRICS_VALUE ("emacs_gc_collections_total",
		  zmetrics_load (&zmetrics_gc_count));
  ZMETRICS_TEXT
    ("# HELP emacs_gc_pause_seconds_total Total time spent in garbage collection.\n"
     "# TYPE emacs_gc_pause_seconds_total counter\n");
  zmetrics_metric_seconds (&buffer, "emacs_gc_pause_seconds_total",
			   zmetrics_load (&zmetrics_gc_total_ns));
  ZMETRICS_TEXT
    ("# HELP emacs_gc_last_pause_seconds Duration of the most recently completed garbage collection.\n"
     "# TYPE emacs_gc_last_pause_seconds gauge\n");
  zmetrics_metric_seconds (&buffer, "emacs_gc_last_pause_seconds",
			   zmetrics_load (&zmetrics_gc_last_ns));
  ZMETRICS_TEXT
    ("# HELP emacs_gc_max_pause_seconds Longest garbage collection completed by this process.\n"
     "# TYPE emacs_gc_max_pause_seconds gauge\n");
  zmetrics_metric_seconds (&buffer, "emacs_gc_max_pause_seconds",
			   zmetrics_load (&zmetrics_gc_max_ns));
  ZMETRICS_TEXT
    ("# HELP emacs_process_uptime_seconds Time since the metrics observer started.\n"
     "# TYPE emacs_process_uptime_seconds gauge\n");
  zmetrics_metric_seconds (&buffer, "emacs_process_uptime_seconds",
			   now - zmetrics_load (&zmetrics_started_ns));
  ZMETRICS_TEXT
    ("# HELP emacs_metrics_http_requests_total HTTP requests accepted by the native metrics listener.\n"
     "# TYPE emacs_metrics_http_requests_total counter\n");
  ZMETRICS_VALUE ("emacs_metrics_http_requests_total",
		  zmetrics_load (&zmetrics_http_requests));

#undef ZMETRICS_TEXT
#undef ZMETRICS_VALUE

  return buffer.length < buffer.capacity ? buffer.length : 0;
}

static void
zmetrics_send_all (int fd, char const *data, size_t length)
{
  while (length)
    {
#ifdef MSG_NOSIGNAL
      ssize_t sent = send (fd, data, length, MSG_NOSIGNAL);
#else
      ssize_t sent = send (fd, data, length, 0);
#endif
      if (sent > 0)
	{
	  data += sent;
	  length -= sent;
	}
      else if (sent < 0 && errno == EINTR)
	continue;
      else
	return;
    }
}

static void
zmetrics_respond (int fd, int status, char const *reason,
		  char const *content_type, char const *body, size_t body_length)
{
  char header[512];
  int header_length
    = snprintf (header, sizeof header,
		"HTTP/1.1 %d %s\r\n"
		"Content-Type: %s\r\n"
		"Content-Length: %zu\r\n"
		"Connection: close\r\n\r\n",
		status, reason, content_type, body_length);
  if (0 < header_length && (size_t) header_length < sizeof header)
    {
      zmetrics_send_all (fd, header, header_length);
      zmetrics_send_all (fd, body, body_length);
    }
}

static void
zmetrics_handle_connection (int fd)
{
  struct timeval timeout = { 2, 0 };
  setsockopt (fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
  setsockopt (fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof timeout);
#ifdef SO_NOSIGPIPE
  int one = 1;
  setsockopt (fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif

  char request[ZMETRICS_REQUEST_CAPACITY];
  size_t used = 0;
  request[0] = '\0';
  while (used < sizeof request - 1)
    {
      ssize_t count = recv (fd, request + used, sizeof request - 1 - used, 0);
      if (count > 0)
	{
	  used += count;
	  request[used] = '\0';
	  if (strstr (request, "\r\n"))
	    break;
	}
      else if (count < 0 && errno == EINTR)
	continue;
      else
	break;
    }

  __atomic_fetch_add (&zmetrics_http_requests, 1, __ATOMIC_RELAXED);

  if (strncmp (request, "GET /metrics ", 13) == 0)
    {
      char body[ZMETRICS_RESPONSE_CAPACITY];
      size_t body_length = zmetrics_render_metrics (body, sizeof body);
      if (body_length)
	zmetrics_respond (fd, 200, "OK",
			  "text/plain; version=0.0.4; charset=utf-8",
			  body, body_length);
      else
	zmetrics_respond (fd, 500, "Internal Server Error", "text/plain",
			  "metrics rendering failed\n", 25);
    }
  else if (strncmp (request, "GET /healthz ", 13) == 0)
    {
      uint64_t now = zmetrics_now_ns ();
      uint64_t checkpoint = zmetrics_load (&zmetrics_checkpoint_ns);
      bool input_wait_active
	= zmetrics_load (&zmetrics_waiting_for_input);
      bool healthy
	= input_wait_active
	  || (checkpoint <= now
	      && now - checkpoint < ZMETRICS_STALL_SECONDS * 1000000000ULL);
      char const *body
	= healthy ? "healthy\n" : "unhealthy: main thread stalled\n";
      zmetrics_respond (fd, healthy ? 200 : 503,
			healthy ? "OK" : "Service Unavailable",
			"text/plain; charset=utf-8", body, strlen (body));
    }
  else
    zmetrics_respond (fd, 404, "Not Found", "text/plain; charset=utf-8",
		      "not found\n", 10);
}

static void *
zmetrics_server_thread (void *argument)
{
  int listener = *(int *) argument;
  sys_thread_set_name ("emacs-zmetrics");

  while (true)
    {
      int connection = accept (listener, NULL, NULL);
      if (connection >= 0)
	{
	  zmetrics_handle_connection (connection);
	  close (connection);
	}
      else if (errno != EINTR)
	break;
    }
  close (listener);
  return NULL;
}

void
zmetrics_start (void)
{
  char const *text = getenv ("ZMETRICS_PORT");
  if (!text || !*text)
    return;

  char *end;
  errno = 0;
  long port = strtol (text, &end, 10);
  if (errno || *end || port < 1 || 65535 < port)
    return;

  int listener = socket (AF_INET, SOCK_STREAM, 0);
  if (listener < 0)
    return;

  int one = 1;
  setsockopt (listener, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
  fcntl (listener, F_SETFD, FD_CLOEXEC);

  struct sockaddr_in address = { 0 };
  address.sin_family = AF_INET;
  address.sin_port = htons (port);
  address.sin_addr.s_addr = htonl (INADDR_LOOPBACK);
  if (bind (listener, (struct sockaddr *) &address, sizeof address) < 0
      || listen (listener, 16) < 0)
    {
      close (listener);
      return;
    }

  zmetrics_started_ns = zmetrics_now_ns ();
  zmetrics_observe_main_thread (false, true);

  static int listener_for_thread;
  listener_for_thread = listener;
  sys_thread_t thread;
  sigset_t all_signals;
  sigset_t saved_signals;
  sigfillset (&all_signals);
  if (pthread_sigmask (SIG_BLOCK, &all_signals, &saved_signals) != 0)
    {
      close (listener);
      return;
    }
  bool created
    = sys_thread_create (&thread, zmetrics_server_thread,
			 &listener_for_thread);
  pthread_sigmask (SIG_SETMASK, &saved_signals, NULL);
  if (!created)
    {
      close (listener);
      return;
    }

}
