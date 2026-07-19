;;; z-remote.el --- push status to k.local and poll for tasks -*- lexical-binding: t -*-

(require 'json)
(require 'cl-lib)

(defvar z-remote-host "k.local"
  "Host where z-server.py runs.")

(defvar z-remote-token-file "~/.z-token"
  "File containing the shared secret for z-server auth.")

(defvar z-remote-port 7700)

(defvar z-remote--token nil "Cached token string.")

(defun z-remote--token ()
  (or z-remote--token
      (let ((f (expand-file-name z-remote-token-file)))
        (when (file-readable-p f)
          (setq z-remote--token (string-trim (with-temp-buffer
                                               (insert-file-contents f)
                                               (buffer-string))))))))

;;; --- status push ---

(defun z-remote--status-json ()
  "Build a JSON string of current z board state."
  (let* ((h (ignore-errors (z--local-health)))
         (kai (ignore-errors (z--kai-focus)))
         (alerts (ignore-errors (z--scan-alerts))))
    (json-encode
     `(("ts"      . ,(float-time))
       ("status"  . ,(or (and h (if (plist-get h :alive) "alive" "wedged")) "unknown"))
       ("pid"     . ,(or (and h (plist-get h :pid)) "?"))
       ("cpu"     . ,(or (and h (plist-get h :cpu)) "?"))
       ("uptime"  . ,(or (and h (plist-get h :uptime)) "?"))
       ("kai"     . ,(or kai "unknown"))
       ("zstatus" . ,z--status)
       ("last"    . ,(or z--last-action "none"))
       ("alerts"  . ,(or alerts []))))))

(defun z-remote-push-status ()
  "Push current z board state to k.local z-server. Non-blocking."
  (when-let* ((tok (z-remote--token))
              (json (ignore-errors (z-remote--status-json)))
              (url (format "http://%s:%d/status-push" z-remote-host z-remote-port)))
    (let ((proc (start-process "z-push" nil
                               "curl" "-s" "--max-time" "5"
                               "-X" "POST"
                               "-H" (concat "X-Z-Token: " tok)
                               "-H" "Content-Type: application/json"
                               "-d" json
                               url)))
      (set-process-sentinel proc #'ignore))))

;;; --- task poll ---

(defvar z-remote--poll-timer nil)

(defun z-remote-poll-tasks ()
  "Poll k.local for queued tasks and dispatch each. Non-blocking."
  (when-let* ((tok (z-remote--token))
              (url (format "http://%s:%d/tasks" z-remote-host z-remote-port)))
    (let* ((buf (generate-new-buffer " *z-poll*"))
           (proc (start-process "z-poll" buf
                                "curl" "-s" "--max-time" "8"
                                "-H" (concat "X-Z-Token: " tok)
                                url)))
      (set-process-sentinel
       proc
       (lambda (p _)
         (when (eq (process-status p) 'exit)
           (unwind-protect
               (with-current-buffer buf
                 (let* ((raw (buffer-string))
                        (tasks (ignore-errors (json-parse-string raw :array-type 'list))))
                   (when (listp tasks)
                     (dolist (task tasks)
                       (let ((text (gethash "task" task)))
                         (when (and text (not (string-empty-p text)))
                           (z-remote--dispatch-task text)))))))
             (kill-buffer buf))))))))

(defun z-remote--dispatch-task (text)
  "Dispatch TEXT as a codex task via kai-board."
  (z-set-last-action (format "task from phone: %s" (truncate-string-to-width text 30)))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) text))
            ((symbol-function 'completing-read) (lambda (_ coll &rest __) (car coll))))
    (ignore-errors (kai-board--codex-dispatch))))

(defun z-remote-start ()
  "Start push-on-render and 30s task poll timer."
  (unless (and z-remote--poll-timer (timerp z-remote--poll-timer))
    (setq z-remote--poll-timer (run-with-timer 30 30 #'z-remote-poll-tasks))))

(defun z-remote-stop ()
  (when (timerp z-remote--poll-timer) (cancel-timer z-remote--poll-timer))
  (setq z-remote--poll-timer nil))

(provide 'z-remote)
