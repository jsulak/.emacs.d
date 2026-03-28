;; Make default text a bit muted
(setq modus-vivendi-palette-overrides
      '((fg-main "#d0d0d0")))
(load-theme 'modus-vivendi t)

(set-face-attribute 'default nil :font "JetBrains Mono-14")

;; window frame title
(setq frame-title-format "Emacs %f")

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)


(provide 'james-gui)

