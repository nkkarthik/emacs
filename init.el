;;; init.el --- Personal Emacs init -*- lexical-binding: t -*-

(with-eval-after-load 'msmail
  (msmail-sync-timer-start 300))
