
;; Add color themes
(setq load-path (append load-path '("~/.emacs.d/themes/")))
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/external/ruby-blue-theme.el")
(color-theme-ruby-blue)

;; Add line highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#19293A")

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Move scroll bar to right
(setq scroll-bar-mode-explicit t) 
(set-scroll-bar-mode `right) 

;; window frame title
(setq frame-title-format "%b (%f) - emacs")
(setq icon-title-format "emacs [%b]")


(provide 'james-gui)