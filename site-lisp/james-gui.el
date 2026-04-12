;;; james-gui.el --- GUI-only settings -*- lexical-binding: t; -*-

(require 'james-theme)

(menu-bar-mode 1)

(setq org-todo-keyword-faces
      '(("ON HOLD" . (:foreground "#a8a8a8" :weight bold))))

;; Default to light; james-osx.el overrides with system-aware switching.
(james/apply-theme 'annex-light)

(set-face-attribute 'default nil :font "JetBrains Mono-15")

;; window frame title
(setq frame-title-format
      '(:eval (let ((path (or buffer-file-name default-directory "")))
                (concat "Emacs " (abbreviate-file-name path)))))

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(setq mouse-wheel-follow-point t)
(setq scroll-conservatively 101)   ; never recenter aggressively
(setq scroll-margin 3)             ; keep 3 lines of context at edges
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))  ; 3 lines per tick
(setq mouse-wheel-progressive-speed nil)  ; disable acceleration


(provide 'james-gui)

