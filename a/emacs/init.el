;;; init.el --- Init File -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'deps) ;; assumes lisp/deps.el with (provide 'deps) at the end


;; (set-face-attribute 'default nil :font "SF Mono-16")
;; (global-tab-line-mode 1)
;;(set-face-attribute 'tab-line nil :family "SF Mono" :height 120)


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
  "pp" '(projectile-switch-project :wk "switch project")
  "pf" '(projectile-find-file :wk "find file in project")
  "pb" '(projectile-switch-to-buffer :wk "switch project buffer")
  
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


(require 'k) ;; assumes lisp/k.el with (provide 'k) at the end

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
