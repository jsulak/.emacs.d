;;(load-theme 'wombat)


;; Mouse behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; window frame title
(setq frame-title-format "Emacs %f")


(provide 'james-gui)
