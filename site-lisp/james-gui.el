;;; james-gui.el --- GUI-only settings -*- lexical-binding: t; -*-

(defvar modus-vivendi-palette-overrides)

;; Make default text a bit muted
(setq modus-vivendi-palette-overrides
      '((fg-main "#d0d0d0")))
(setq org-todo-keyword-faces
      '(("ON HOLD" . (:foreground "#a8a8a8" :weight bold))))
(load-theme 'modus-vivendi t)

(set-face-attribute 'default nil :font "JetBrains Mono-14")

;; window frame title
(setq frame-title-format
      '(:eval (let ((path (or buffer-file-name default-directory "")))
                (concat "Emacs " (abbreviate-file-name path)))))

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)


(provide 'james-gui)

