(load-theme 'modus-vivendi t)

;; Make default text a bit muted
(setq modus-vivendi-palette-overrides                                                                                 '((fg-main "#d0d0d0")))

(set-face-attribute 'default nil :font "JetBrains Mono-14")

;; Mouse behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; window frame title
(setq frame-title-format "Emacs %f")


(provide 'james-gui)

