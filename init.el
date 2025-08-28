;;; init.el --- Init File -*- lexical-binding: t -*-
(setq straight-base-dir "~/.cache/straight/")
(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file "~/.cache/straight/straight/repos/straight.el/bootstrap.el")
       ;; (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))


(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))


(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  ; :bind (("M-[" . paredit-wrap-square)
  ;       ("M-{" . paredit-wrap-curly))
                                        ; :diminish nil
  )


;; Magit - Git interface
(use-package magit
  :commands magit-status)


;; Eglot - Built-in LSP client (Emacs 29+)
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (javascript-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (go-mode . eglot-ensure)))


;;; use-package END


;; koptions
;; (add-to-list 'load-path (expand-file-name "a" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/k"))

(load "k" :noerror)
;; koptions END




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
