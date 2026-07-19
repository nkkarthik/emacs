;;; z.el --- z board: meta-master oversight panel -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'z-remote nil t)

;;; --- config ---
(defvar z--status "idle")
(defvar z--last-action nil)
(defvar z--timer nil)
(defvar z--remote-timer nil)
(defvar z--remote-health nil "Alist of (HOST . ALIVE-bool) for remote machines.")
(defvar z-ping--next nil "Float-time when next ping fires, or nil.")

(defvar z--managed-sessions
  '(("claude:a" . "book")
    ("codex:e"  . "book")
    ("codex:e"  . "k.local")
    ("codex:e"  . "m.local"))
  "Sessions I manage: (SESSION-NAME . HOST).")

(defvar z--all-hosts
  '("book" "m.local" "k.local" "mnk.local" "ksys.local")
  "All fleet machines to monitor.")

;;; --- health checks ---
(defun z--local-health ()
  "Check main daemon on book. Return plist :pid :cpu :uptime :alive."
  (let* ((pid (string-trim (shell-command-to-string
                            "pgrep -f 'emacs --fg-daemon$' 2>/dev/null")))
         (pid (when (string-match-p "^[0-9]+$" pid) pid))
         (ps (when pid (string-trim (shell-command-to-string
                                     (format "ps -p %s -o pid=,pcpu=,etime= 2>/dev/null" pid)))))
         (ping (shell-command-to-string
                "emacsclient --timeout 3 -e '(emacs-version)' 2>&1"))
         (alive (string-match-p "GNU Emacs" ping))
         (parts (when ps (split-string ps))))
    (list :pid (or pid "?")
          :cpu (or (nth 1 parts) "?")
          :uptime (or (nth 2 parts) "?")
          :alive alive)))

(defun z--remote-ping (host)
  "Return t if emacs daemon is running on HOST (via ssh pgrep)."
  (= 0 (call-process "ssh" nil nil nil
                     "-o" "ConnectTimeout=3"
                     "-o" "BatchMode=yes"
                     host "pgrep" "-x" "emacs")))

(defun z--refresh-remote ()
  "Refresh z--remote-health for all non-book hosts. Runs on 5-min timer."
  (dolist (host (cdr z--all-hosts)) ; skip "book"
    (let ((alive (z--remote-ping host)))
      (setf (alist-get host z--remote-health nil nil #'equal) alive)))
  (ignore-errors (z-render)))

(defun z--kai-focus ()
  "Return kai's current status/focus from main daemon."
  (let* ((out (string-trim (shell-command-to-string
               "emacsclient --timeout 5 -e \
\"(when (boundp 'kai-board-state) \
  (let ((now (plist-get kai-board-state :now)) \
        (st  (plist-get kai-board-state :status))) \
    (or now st)))\" 2>&1")))
         (s out))
    (if (or (string= s "") (string= s "nil") (string-prefix-p "*ERROR*" s))
        "unknown"
      (string-trim s "\"" "\""))))

;;; --- buffer health ---
(defun z--scan-alerts ()
  "Scan *Messages* and *Warnings* in the z daemon for errors. Return list of strings."
  (let ((alerts '()))
    (dolist (bname '("*Messages*" "*Warnings*" "*Compile-Log*" "*Backtrace*"))
      (when-let* ((buf (get-buffer bname)))
        (with-current-buffer buf
          (let ((text (buffer-substring-no-properties
                       (max (point-min) (- (point-max) 500)) (point-max))))
            (dolist (line (split-string text "\n" t))
              (when (string-match-p
                     "\\(error\\|Error\\|warning\\|Warning\\|Backtrace\\|void\\|Wrong\\)" line)
                (push (truncate-string-to-width (string-trim line) 36) alerts)))))))
    (seq-take (delete-dups (nreverse alerts)) 5)))

;;; --- render ---
(defun z--status-glyph (alive)
  (if alive
      (propertize "●" 'face 'success)
    (propertize "✗" 'face 'warning)))

(defun z-render ()
  "Redraw the *z* board."
  (with-current-buffer (get-buffer-create "*z*")
    (unless (derived-mode-p 'z-mode) (z-mode))
    (let* ((inhibit-read-only t)
           (h (z--local-health))
           (sep (propertize "────────────────────\n" 'face 'shadow)))
      (erase-buffer)
      ;; header
      (insert (propertize "*z*" 'face 'bold))
      (insert (propertize (format "  [%s]\n" z--status)
                          'face (if (equal z--status "working") 'warning 'shadow)))
      (insert sep)
      ;; sessions I manage
      (insert (propertize "SESSIONS\n" 'face 'shadow))
      (dolist (s z--managed-sessions)
        (let* ((sname (car s))
               (host (cdr s))
               (alive (if (equal host "book")
                          (plist-get h :alive)
                        (alist-get host z--remote-health nil nil #'equal))))
          (insert (format "  %s %-12s %s\n"
                          (z--status-glyph alive) host sname))))
      (insert sep)
      ;; local daemon detail
      (insert (propertize "BOOK DAEMON\n" 'face 'shadow))
      (insert (format "  pid %s  cpu %s%%  up %s\n"
                      (plist-get h :pid)
                      (plist-get h :cpu)
                      (plist-get h :uptime)))
      (insert (if (plist-get h :alive)
                  (propertize "  ALIVE\n" 'face 'success)
                (propertize "  WEDGED\n" 'face 'warning)))
      (insert sep)
      ;; remote daemons
      (insert (propertize "FLEET DAEMONS\n" 'face 'shadow))
      (dolist (host (cdr z--all-hosts))
        (let ((alive (alist-get host z--remote-health nil nil #'equal)))
          (insert (format "  %s %s\n" (z--status-glyph alive) host))))
      (insert sep)
      ;; kai focus
      (insert (propertize "KAI NOW\n" 'face 'shadow))
      (insert (format "  %s\n" (z--kai-focus)))
      (insert sep)
      ;; last action
      (insert (propertize "LAST ACTION\n" 'face 'shadow))
      (insert (format "  %s\n" (or z--last-action "none")))
      (insert sep)
      ;; alerts from buffer scan
      (let ((alerts (z--scan-alerts)))
        (when alerts
          (insert (propertize "ALERTS\n" 'face 'warning))
          (dolist (a alerts)
            (insert (propertize (format "  ! %s\n" a) 'face 'warning)))
          (insert sep)))
      ;; ping countdown
      (insert (propertize "PING\n" 'face 'shadow))
      (insert (format "  next: %s\n"
                      (if z-ping--next
                          (format "%.0fs" (max 0 (- z-ping--next (float-time))))
                        "off")))
      (insert sep)
      (insert (propertize "g refresh  h health  r remote  c claude  q quit\n" 'face 'shadow))))
  (when (fboundp 'z-remote-push-status) (z-remote-push-status)))

;;; --- mode ---
(define-derived-mode z-mode special-mode "Z"
  "Meta-master oversight board.")
(define-key z-mode-map (kbd "g") #'z-refresh)
(define-key z-mode-map (kbd "h") #'z-health-check)
(define-key z-mode-map (kbd "r") #'z-remote-check)
(define-key z-mode-map (kbd "c") #'z-open-claude)
(define-key z-mode-map (kbd "q") #'bury-buffer)

;;; --- API ---
(defun z-refresh () (interactive) (z-render))

(defun z-health-check ()
  "Check local daemon health."
  (interactive)
  (let ((h (z--local-health)))
    (unless (plist-get h :alive)
      (z-set-last-action "ALERT: main daemon WEDGED"))
    (z-render)))

(defun z-remote-check ()
  "Manually trigger remote fleet health check."
  (interactive)
  (z-set-last-action "checking remote fleet...")
  (z--refresh-remote))

(defun z-show ()
  (interactive)
  (display-buffer-in-side-window
   (get-buffer-create "*z*")
   '((side . right) (slot . 1) (window-width . 0.3)))
  (z-render))

(defun z-ensure ()
  "Start timers and show board."
  (unless (and z--timer (timerp z--timer))
    (setq z--timer (run-with-timer 30 30 #'z-render)))
  (unless (and z--remote-timer (timerp z--remote-timer))
    (setq z--remote-timer (run-with-timer 60 300 #'z--refresh-remote)))
  (z-ping-start)
  (when (fboundp 'z-remote-start) (z-remote-start))
  (z-show))

(defun z-open-claude ()
  "Switch to the *claude:e* session buffer."
  (interactive)
  (let ((buf (or (get-buffer "*claude:e*") (get-buffer "*claude:z*"))))
    (if buf
        (switch-to-buffer buf)
      (message "z: no claude session buffer found"))))

(defun z-set-last-action (text)
  (setq z--last-action text)
  (ignore-errors (z-render)))


;;; --- z-ping: recurring heartbeat to *claude:e* ---

(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(defvar z-ping-interval 300
  "Seconds between pings to *claude:e*. Default 5 min.")

(defvar z-ping-text "continue"
  "Text injected into *claude:e* on each ping.")

(defvar z-ping--timer nil)
(defvar z-ping--next nil "Float-time when next ping fires, or nil.")

(defun z-ping ()
  "Send z-ping-text + RET to *claude:e* if main daemon is idle.
Callable externally: emacsclient --socket-name z -f z-ping"
  (interactive)
  (let* ((status-raw (string-trim
                      (shell-command-to-string
                       "emacsclient --timeout 3 -e \"(plist-get kai-board-state :status)\" 2>&1")))
         (idle (or (string= status-raw "nil")
                   (string= status-raw "\"idle\"")
                   (string= status-raw "idle")))
         (buf (get-buffer "*claude:e*")))
    (if (and idle buf)
        (with-current-buffer buf
          (if (fboundp 'vterm-send-string)
              (progn
                (vterm-send-string z-ping-text)
                (vterm-send-return)
                (z-set-last-action (format "ping sent: %s" z-ping-text)))
            (message "z-ping: *claude:e* is not a vterm")))
      (z-set-last-action (format "ping skipped: status=%s buf=%s"
                                 status-raw (if buf "found" "missing"))))))

(defun z-ping-start (&optional interval)
  "Start the recurring ping timer. INTERVAL overrides z-ping-interval."
  (z-ping-stop)
  (let ((secs (or interval z-ping-interval)))
    (setq z-ping--next (+ (float-time) secs)
          z-ping--timer
          (run-with-timer secs secs
            (lambda ()
              (setq z-ping--next (+ (float-time) z-ping-interval))
              (z-ping)
              (ignore-errors (z-render)))))))

(defun z-ping-stop ()
  "Cancel the ping timer."
  (when (timerp z-ping--timer) (cancel-timer z-ping--timer))
  (setq z-ping--timer nil z-ping--next nil))

(provide 'z)
