;; CRITICAL: Prevent multiple frames entirely
;; (setq pop-up-frames nil)
;; (setq pop-up-windows nil)
;; (setq display-buffer-reuse-frames nil)
;; (setq frame-auto-hide-function 'delete-frame)

;; ;; Force single window behavior
;; (setq window-combination-resize t)
;; (setq split-height-threshold nil)
;; (setq split-width-threshold nil)

;; Change interface elements
;; (menu-bar-mode -1)
(global-tab-line-mode 1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(fringe-mode 0)

;; Remove window decorations completely
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; Disable startup elements
;; (setq inhibit-startup-screen t)
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message nil)

;; Prevent frame creation for help, completions, etc.
;; (setq help-window-select t)
;; (setq completion-auto-help nil)

;; Force everything to use current window
;; (setq display-buffer-alist
;;       '((".*" (display-buffer-same-window))))

;; Pixel-perfect resizing
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; Disable cursor blinking for performance
;; (blink-cursor-mode -1)

;; Set background to pure black
;; (set-background-color "black")
;; (set-foreground-color "green")
