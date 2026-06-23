;;; init.el --- Personal Emacs init -*- lexical-binding: t -*-

(use-package compat
  :ensure t)

(use-package emacsql
  :ensure t)

(use-package magit-section
  :ensure t)

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam")
  :config
  (org-roam-db-autosync-mode))

(with-eval-after-load 'msmail
  (msmail-sync-timer-start 300))
