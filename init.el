;;; init.el --- Init File -*- lexical-binding: t -*-

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'deps) ;; assumes lisp/deps.el with (provide 'deps) at the end
;; (provide 'k)

;; (set-face-attribute 'default nil :font "SF Mono-16")
;; (global-tab-line-mode 1)
;;(set-face-attribute 'tab-line nil :family "SF Mono" :height 120)



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
(use-package vterm)

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

(with-eval-after-load 'treemacs
  (with-eval-after-load 'treemacs
    (evil-define-key 'normal treemacs-mode-map (kbd "RET") #'treemacs-RET-action)
    (evil-define-key 'emacs treemacs-mode-map (kbd "RET") #'treemacs-RET-action)))


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
  :init
  (vertico-mode))
;; Orderless - Better completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult - Enhanced search and navigation commands
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c h" . consult-history))
  :config
  (leader-def
    "bb" 'consult-buffer
    "hh" 'consult-history))

;; Embark - Contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Corfu - In-buffer completion popup
(use-package corfu
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
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Magit - Git interface
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; Evil integration for Magit
(use-package evil-magit
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
  :config
  (setq inferior-lisp-program "sbcl"))


(use-package paredit
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
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


;;; org

;; (require 'org-tempo)

(setq org-confirm-babel-evaluate nil)
(setq org-babel-lisp-eval-fn #'sly-eval)

;; Original value was ((emacs-lisp . t))
;; (org-babel-do-load-languages 'org-babel-load-languages
;;                             '((lisp . t)))

;;; k funs

(defun k/maybe-delete-frame ()
  "Delete frame on Cmd+Q if running as daemon, otherwise kill terminal."
  (interactive)
  (if (and (daemonp) (not (eq (selected-frame) (car (frame-list)))))
      (delete-frame)
    (save-buffers-kill-terminal)))

;; Remap Cmd+Q behavior (GUI only)
(global-set-key (kbd "s-q") #'k/maybe-delete-frame)


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


(defun k/vpn ()
  "connect this machine to vpn"
  (interactive)
  (async-shell-command "make -C ~/m/vpn" "*k*"))

(defun k/vpnk ()
  "connect k machine to vpn"
  (interactive)
  (async-shell-command "make -C ~/m/vpn k" "*k*"))


;; options
(tool-bar-mode 0)
(xterm-mouse-mode 1)

;; Enhanced leader key bindings
(leader-def
  ;; Files
  "f" '(:ignore t :wk "files")
  "ff" '(find-file :wk "find file")
  "fr" '(consult-recent-file :wk "recent files")
  "fs" '(save-buffer :wk "save file")
  ;; Buffers
  "b" '(:ignore t :wk "buffers")
  "bb" '(consult-buffer :wk "switch buffer")
  "bk" '(kill-buffer :wk "kill buffer")
  "br" '(revert-buffer :wk "revert buffer")
  ;; Search
  "s" '(:ignore t :wk "search")
  "ss" '(consult-line :wk "search buffer")
  "sp" '(consult-ripgrep :wk "search project")
  ;; Project
  "p" '(:ignore t :wk "project")
  "pp" '(project-switch-project :wk "switch project")
  "pf" '(project-find-file :wk "find file in project")
  "pb" '(project-switch-to-buffer :wk "switch project buffer")
  ;; Git
  "g" '(:ignore t :wk "git")
  "gs" '(magit-status :wk "git status")
  "gb" '(magit-branch :wk "git branch")
  "gc" '(magit-commit :wk "git commit")
  ;; Windows
  "w" '(:ignore t :wk "windows")
  "wh" '(windmove-left :wk "move left")
  "wj" '(windmove-down :wk "move down")
  "wk" '(windmove-up :wk "move up")
  "wl" '(windmove-right :wk "move right")
  "ws" '(split-window-below :wk "split below")
  "wv" '(split-window-right :wk "split right")
  "wd" '(delete-window :wk "delete window")
  ;; Open
  "o" '(:ignore t :wk "open")
  "op" '(treemacs :wk "treemacs")
  "ot" '(treemacs-select-window :wk "treemacs window")
  ;; Help
  "h" '(:ignore t :wk "help")
  "hf" '(describe-function :wk "describe function")
  "hv" '(describe-variable :wk "describe variable")
  "hk" '(describe-key :wk "describe key"))


;; themes
(use-package modus-themes
  :config
  (load-theme 'modus-operandi t)   ; light
)

;; (use-package modus-themes
;;   :config
;;   ;; Optional customizations before loading
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs  t
;;         modus-themes-headings        '((t . rainbow))
;;         modus-themes-variable-pitch-headings nil)
;;   ;; Load light or dark variant:
;;   (load-theme 'modus-operandi t)   ; light
;;   ;;(load-theme 'modus-vivendi   t) ; dark
;;   ;; Define a toggle function
;;   (defun my/toggle-modus-theme ()
;;     (interactive)
;;     (if (member 'modus-operandi custom-enabled-themes)
;;         (load-theme 'modus-vivendi t)
;;       (load-theme 'modus-operandi t)))
;;   :bind ("<f5>" . my/toggle-modus-theme))


(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


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
