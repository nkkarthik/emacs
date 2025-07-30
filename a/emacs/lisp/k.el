
;; k funs

(defvar k-action-prev "" "holds previous k/action to repeat later")

(defun k/action ()
  "Run the current line in an asynchronous shell (`async-shell-command`)."
  (interactive)
  (let* ((line
	  (if (use-region-p)
	      (buffer-substring (region-beginning) (region-end))
	    (thing-at-point 'line t)))
         (cmd (string-trim line)))
    (if (string-empty-p cmd)
        (message "Current line is empty")
      (setq k-action-prev cmd)
      (async-shell-command cmd "*k*"))))


(defun k/action-repeat ()
  "Repeat previous k/action"
  (interactive)
  (unless (string-empty-p k-action-prev)
    (async-shell-command k-action-prev "*k*")))


(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-j") #'k/action))
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-k") #'k/action-repeat))




(provide 'k)
