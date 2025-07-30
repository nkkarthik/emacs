;;; -*- lexical-binding: t; -*-

(require 'widget)

(defvar f1)
(defvar f2)
(defvar fr)

(defun sumthem (widget &rest ignore)
  "sum widget f1 f2 to fr"
  (let ((n1 (widget-value f1))
        (n2 (widget-value f2))
        res)
    (setq res (+ (string-to-number n1) (string-to-number n2)))
    (widget-value-set fr (number-to-string res))))

(defun sumcalc ()
  "display simple calc to sum two numbers"
  (interactive)
  (switch-to-buffer "*sumcalc*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (remove-overlays)
  (widget-insert "sumcalc\n\n")
  (setq f1 (widget-create 'editable-field
                          :size 5
                          :help-echo "one"
                          :format "number 1: %v"))
  (widget-insert "\n\n")
  (setq f2 (widget-create 'editable-field
                          :size 5
                          :help-echo "two"
                          :format "number 2: %v"))
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify #'sumthem
                 :help-echo "sumthem"
                 :highlight t
                 "sumthem")
  (widget-insert "\n\n")
  (setq fr (widget-create 'item
                          :format "result: %v"))
  (use-local-map widget-keymap)
  (widget-setup)
  (widget-forward 1))
