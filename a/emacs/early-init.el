;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Move native compilation cache out of .emacs.d (to ~/.cache)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "emacs/eln" (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
;; Move URL cache/config out of .emacs.d
(setq url-configuration-directory
      (expand-file-name "emacs/url/" (or (getenv "XDG_CACHE_HOME") "~/.cache")))


;; Default UI typeface
;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono 13")) ; 13 pt ≈ 173 dpi on Retina[14]
(add-to-list 'default-frame-alist '(font . "Fira Code-13")) ; 13 pt ≈ 173 dpi on Retina[14]
(setq-default line-spacing 0.2)               ; 0.2× height adds ~20 %[14]













;; Disable startup screen and messages
;; (setq inhibit-startup-screen t
;;       inhibit-startup-message t
;;       inhibit-startup-echo-area-message t
;;       initial-scratch-message nil)

;; Disable GUI elements
;; (setq menu-bar-mode nil
;;       tool-bar-mode nil
;;       scroll-bar-mode nil)

;; Frame settings
;; (setq frame-inhibit-implied-resize t
;;       frame-title-format nil
;;       cursor-in-non-selected-windows nil)

;; Increase garbage collection threshold for faster startup
;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6)

;; Reset garbage collection after startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold 16777216
;;                  gc-cons-percentage 0.1)))

