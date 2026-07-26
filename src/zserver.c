/* Loopback z-server C skeleton.
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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "lisp.h"
#include "systhread.h"
#include "zserver.h"

static time_t zserver_started_at = 0;
static pid_t zserver_pid = 0;
static int zserver_listener = -1; /* For clean shutdown later.  */

static ssize_t
zserver_send_all (int fd, char const *buf, size_t n)
{
  ssize_t total = 0;
  while (n)
    {
#ifdef MSG_NOSIGNAL
      ssize_t sent = send (fd, buf, n, MSG_NOSIGNAL);
#else
      ssize_t sent = send (fd, buf, n, 0);
#endif
      if (sent > 0)
	{
	  buf += sent;
	  n -= sent;
	  total += sent;
	}
      else if (sent < 0 && errno == EINTR)
	continue;
      else
	return -1;
    }
  return total;
}

static void
zserver_respond (int fd, int status, char const *reason,
		 char const *content_type, char const *body,
		 size_t body_length)
{
  char header[512];
  int header_length
    = snprintf (header, sizeof header,
		"HTTP/1.0 %d %s\r\n"
		"Content-Type: %s\r\n"
		"Content-Length: %zu\r\n"
		"Connection: close\r\n\r\n",
		status, reason, content_type, body_length);
  if (0 < header_length && (size_t) header_length < sizeof header)
    {
      zserver_send_all (fd, header, header_length);
      zserver_send_all (fd, body, body_length);
    }
}

static void
zserver_handle_ping (int fd)
{
  char body[256];
  time_t now = time (NULL);
  int body_length
    = snprintf (body, sizeof body,
		"{\"ok\":true,\"ts\":%lld,\"build_id\":\"c-skeleton\","
		"\"pid\":%lld,\"started_at\":%lld}",
		(long long) now, (long long) zserver_pid,
		(long long) zserver_started_at);
  if (0 <= body_length && (size_t) body_length < sizeof body)
    zserver_respond (fd, 200, "OK", "application/json", body, body_length);
  else
    zserver_respond (fd, 500, "Internal Server Error",
		     "text/plain; charset=utf-8", "ping rendering failed\n",
		     22);
}

static void
zserver_handle_connection (int fd)
{
  struct timeval timeout = { 2, 0 };
  setsockopt (fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
  setsockopt (fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof timeout);
#ifdef SO_NOSIGPIPE
  int one = 1;
  setsockopt (fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif

  char request[2049];
  size_t used = 0;
  request[0] = '\0';
  while (used < sizeof request - 1)
    {
      ssize_t count = recv (fd, request + used,
			    sizeof request - 1 - used, 0);
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

  if (strncmp (request, "GET /ping ", 10) == 0)
    zserver_handle_ping (fd);
  else
    zserver_respond (fd, 404, "Not Found", "text/plain; charset=utf-8",
		     "not found\n", 10);
}

static void *
zserver_server_thread (void *argument)
{
  int listener = *(int *) argument;
  sys_thread_set_name ("emacs-zserver");

  while (true)
    {
      int connection = accept (listener, NULL, NULL);
      if (connection >= 0)
	{
	  zserver_handle_connection (connection);
	  close (connection);
	}
      else if (errno != EINTR)
	break;
    }
  close (listener);
  return NULL;
}

void
zserver_start (void)
{
  char const *text = getenv ("ZSERVER_PORT");
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

  zserver_started_at = time (NULL);
  zserver_pid = getpid ();
  zserver_listener = listener;

  sys_thread_t thread;
  sigset_t all_signals;
  sigset_t saved_signals;
  sigfillset (&all_signals);
  if (pthread_sigmask (SIG_BLOCK, &all_signals, &saved_signals) != 0)
    {
      close (listener);
      zserver_listener = -1;
      return;
    }
  bool created
    = sys_thread_create (&thread, zserver_server_thread, &zserver_listener);
  pthread_sigmask (SIG_SETMASK, &saved_signals, NULL);
  if (!created)
    {
      close (listener);
      zserver_listener = -1;
      return;
    }
}
