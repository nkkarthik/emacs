;;; -*- lexical-binding: t -*-
;;; Daemon-safe overrides for two subr.el blocking reads.
;;; Loaded by mac/book/init.el after normal startup; redefines the
;;; function cells so the daemon never blocks indefinitely on a prompt.

(defvar kai-daemon-prompt-timeout 10
  "Seconds before an unattended daemon prompt auto-answers.
When (daemonp) is non-nil and this is set,
`read-char-choice-with-read-key' and `read-char-from-minibuffer'
wrap their blocking reads with `with-timeout', returning (car CHARS)
after this many seconds.  Set to nil to disable.")

(defun read-char-choice-with-read-key (prompt chars &optional inhibit-keyboard-quit)
  "Read and return one of the characters in CHARS, prompting with PROMPT.
CHARS should be a list of single characters.
Any input that is not one of CHARS is ignored.

If optional argument INHIBIT-KEYBOARD-QUIT is non-nil, ignore
`keyboard-quit' events while waiting for valid input.

If you bind the variable `help-form' to a non-nil value
while calling this function, then pressing `help-char'
causes it to evaluate `help-form' and display the result.

In daemon mode with `kai-daemon-prompt-timeout' set, times out after
that many seconds and returns the first element of CHARS."
  (unless (consp chars)
    (error "Called `read-char-choice' without valid char choices"))
  (let ((do-read
         (lambda ()
           (let (char done show-help (helpbuf " *Char Help*"))
             (let ((cursor-in-echo-area t)
                   (executing-kbd-macro executing-kbd-macro)
                   (esc-flag nil))
               (save-window-excursion
                 (while (not done)
                   (unless (get-text-property 0 'face prompt)
                     (setq prompt (propertize prompt 'face 'minibuffer-prompt)))
                   (frame-toggle-on-screen-keyboard (selected-frame) nil)
                   (setq char (let ((inhibit-quit inhibit-keyboard-quit))
                                (read-key prompt)))
                   (and show-help (buffer-live-p (get-buffer helpbuf))
                        (kill-buffer helpbuf))
                   (cond
                    ((not (numberp char)))
                    ((and help-form
                          (eq char help-char)
                          (setq show-help t)
                          (help-form-show)))
                    ((memq char chars)
                     (setq done t))
                    ((not inhibit-keyboard-quit)
                     (cond
                      ((and (null esc-flag) (eq char ?\e))
                       (setq esc-flag t))
                      ((memq char '(?\C-g ?\e))
                       (keyboard-quit))))
                    (t
                     (beep)
                     (message "Please type %s"
                              (substitute-command-keys
                               (mapconcat (lambda (c)
                                            (format "\\`%s'"
                                                    (single-key-description c)))
                                          chars ", ")))
                     (sit-for 3))))))
             (message "%s%s" prompt (char-to-string char))
             char))))
    (if (and (daemonp) kai-daemon-prompt-timeout)
        (with-timeout (kai-daemon-prompt-timeout
                       (prog1 (car chars)
                         (message "daemon: prompt timed out after %ds: %s"
                                  kai-daemon-prompt-timeout prompt)))
          (funcall do-read))
      (funcall do-read))))

(defun read-char-from-minibuffer (prompt &optional chars history)
  "Read a character from the minibuffer, prompting for it with PROMPT.
Like `read-char', but uses the minibuffer to read and return a character.
Optional argument CHARS, if non-nil, should be a list of characters;
the function will ignore any input that is not one of CHARS.
Optional argument HISTORY, if non-nil, should be a symbol that
specifies the history list variable to use for navigating in input
history using \`M-p' and \`M-n', with \`RET' to select a character from
history.
If you bind the variable `help-form' to a non-nil value
while calling this function, then pressing `help-char'
causes it to evaluate `help-form' and display the result.
There is no need to explicitly add `help-char' to CHARS;
`help-char' is bound automatically to `help-form-show'.

In daemon mode with `kai-daemon-prompt-timeout' set, times out after
that many seconds and returns the first element of CHARS."
  (when (and (bound-and-true-p overriding-text-conversion-style)
             (bound-and-true-p text-conversion-style))
    (force-mode-line-update))
  (let ((do-read
         (lambda ()
           (let* ((map (if (consp chars)
                           (or (gethash (list help-form (cons help-char chars))
                                        read-char-from-minibuffer-map-hash)
                               (let ((map (make-sparse-keymap))
                                     (msg help-form))
                                 (set-keymap-parent map read-char-from-minibuffer-map)
                                 (when help-form
                                   (define-key map (vector help-char)
                                               (lambda ()
                                                 (interactive)
                                                 (let ((help-form msg))
                                                   (help-form-show)))))
                                 (dolist (char chars)
                                   (define-key map (vector char)
                                               #'read-char-from-minibuffer-insert-char))
                                 (define-key map [remap self-insert-command]
                                             #'read-char-from-minibuffer-insert-other)
                                 (puthash (list help-form (cons help-char chars))
                                          map read-char-from-minibuffer-map-hash)
                                 map))
                         read-char-from-minibuffer-map))
                  (this-command this-command)
                  (result (minibuffer-with-setup-hook
                              (lambda ()
                                (setq-local post-self-insert-hook nil)
                                (add-hook 'post-command-hook
                                          (lambda ()
                                            (if (<= (1+ (minibuffer-prompt-end))
                                                   (point-max))
                                                (exit-minibuffer)))
                                          nil 'local))
                            (progn
                              (when (fboundp 'set-text-conversion-style)
                                (set-text-conversion-style text-conversion-style))
                              (read-from-minibuffer prompt nil map nil (or history t)))))
                  (char
                   (if (plusp (length result))
                       (elt result 0)
                     (when history (push "\r" (symbol-value history)))
                     ?\r)))
             (message "%s%s" prompt (char-to-string char))
             char))))
    (if (and (daemonp) kai-daemon-prompt-timeout)
        (with-timeout (kai-daemon-prompt-timeout
                       (prog1 (car chars)
                         (message "daemon: prompt timed out after %ds: %s"
                                  kai-daemon-prompt-timeout prompt)))
          (funcall do-read))
      (funcall do-read))))
