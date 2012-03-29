
;; Add color themes
;(setq load-path (append load-path '("~/.emacs.d/themes/")))
;(require 'color-theme)
;(color-theme-initialize)
;;(load-file "~/.emacs.d/external/ruby-blue-theme.el")
;;(color-theme-ruby-blue)
;;(color-theme-tangotango)
;; (load-file "~/.emacs.d/external/color-theme-solarized.el")
;; (color-theme-solarized-dark)
;; (load-file "~/.emacs.d/external/naquadah-theme.el")
;; (color-theme-naquadah)

(tool-bar-mode 0)

;; Add line highlighting
(global-hl-line-mode 1)

;; TODO
;;(set-face-background 'hl-line "#19293A")
;(set-face-background 'hl-line "#222626")

;; USUAL:
;; (set-face-background 'hl-line "#2B3030")

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Move scroll bar to right
;; (setq scroll-bar-mode-explicit t)  ::)
;; (set-scroll-bar-mode `right)  ::)

;; window frame title
(setq frame-title-format "emacs - %b (%f)")
(setq icon-title-format "emacs [%b]")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "#162433" :foreground "#C7D4E2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 72 :width normal :foundry "microsoft" :family "Consolas"))))
 '(cperl-array-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(cperl-hash-face ((((class color) (background dark)) (:slant italic :weight bold))))
 '(diredp-date-time ((t nil)))
 '(diredp-dir-heading ((t (:foreground "white"))))
 '(diredp-dir-priv ((t nil)))
 '(diredp-exec-priv ((t nil)))
 '(diredp-file-name ((t nil)))
 '(diredp-file-suffix ((t nil)))
 '(diredp-flag-mark ((t (:foreground "Yellow"))))
 '(diredp-flag-mark-line ((t nil)))
 '(diredp-link-priv ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-other-priv ((t nil)))
 '(diredp-rare-priv ((t nil)))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil)))
; '(ido-subdir ((((min-colors 88) (class color)) (:foreground "orange"))))
 '(js2-jsdoc-html-tag-name-face ((((class color) (min-colors 8) (background dark)) nil))))




(provide 'james-gui)
