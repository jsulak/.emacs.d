;;; james-gui.el --- GUI-only settings -*- lexical-binding: t; -*-

;; Register custom theme directory
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(setq org-todo-keyword-faces
      '(("ON HOLD" . (:foreground "#a8a8a8" :weight bold))))
(load-theme 'annex-light t)

;; Prefer iA Writer Mono S (Annex's font), fall back to JetBrains Mono
(set-face-attribute 'default nil :font
                    (if (find-font (font-spec :name "iA Writer Mono S"))
                        "iA Writer Mono S-15"
                      "JetBrains Mono-15"))

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

