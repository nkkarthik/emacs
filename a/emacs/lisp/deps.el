;;; deps.el - use straight to install packages

;; Move native compilation cache out of .emacs.d (to ~/.cache)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "emacs/eln" (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
;; Move URL cache/config out of .emacs.d
(setq url-configuration-directory
      (expand-file-name "emacs/url/" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
      
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

;; vterm is a real terminal using libvterm
(setq vterm-always-compile-module t)
(use-package vterm :ensure t)

;; Evil mode configuration
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; Evil collection for better evil integration
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; General.el for leader key setup
(use-package general
  :after evil
  :config
  (general-evil-setup)
  
  ;; Set SPC as global leader key
  (general-create-definer leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"))

;; Treemacs file explorer
(use-package treemacs
  :config
  (setq treemacs-width 30
        treemacs-follow-after-init t
        treemacs-is-never-other-window t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))

;; Leader key bindings
(leader-def
  "op" 'treemacs
  "ot" 'treemacs-select-window)

;; which-key - Shows available keybindings in popup
;; Built into Emacs 30, no installation needed
(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-side-window-max-width 0.33))

;; Vertico - Better vertical completion UI
(use-package vertico
  :straight t
  :init
  (vertico-mode))
;; Orderless - Better completion matching
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; Consult - Enhanced search and navigation commands
(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("C-c h" . consult-history))
  :config
  (leader-def
    "bb" 'consult-buffer
    "hh" 'consult-history))

;; Embark - Contextual actions
(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Corfu - In-buffer completion popup
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  :init
  (global-corfu-mode))

;; Cape - Additional completion backends
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Magit - Git interface
(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; Evil integration for Magit
(use-package evil-magit
  :straight t
  :after (evil magit))


;; Eglot - Built-in LSP client (Emacs 29+)
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (javascript-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (java-mode . eglot-ensure))
  :config
  ;; Add language server configurations as needed
  )

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'deps)
