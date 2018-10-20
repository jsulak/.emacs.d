(load-theme 'wombat)

(global-hl-line-mode 1)
(set-fringe-style 6)

;; Mouse behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; TODO
;;(set-face-background 'hl-line "#19293A")
;(set-face-background 'hl-line "#222626")

(set-face-background 'highlight "#303030")
(set-face-underline 'hl-line nil)
(set-face-underline 'highlight nil)
(set-face-foreground 'highlight nil)

;; USUAL:
;; (set-face-background 'hl-line "#2B3030")

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; window frame title
(setq frame-title-format "Emacs %f")


(provide 'james-gui)
