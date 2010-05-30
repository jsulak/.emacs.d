(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#162433" :foreground "#C7D4E2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "Consolas"))))
 '(cperl-array-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(cperl-hash-face ((((class color) (background dark)) (:slant italic :weight bold))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "orange"))))
 '(js2-jsdoc-html-tag-name-face ((((class color) (min-colors 8) (background dark)) nil)))
 '(linum ((t (:inherit (shadow default) :foreground "darkgray")))))

;;Set the default font
(setq default-frame-alist
      '((font .
"-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
        (vertical-scroll-bars . right)))


;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).
;;
(setq exec-path (cons "C:/cygwin/bin" exec-path))
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))

;;
;; NT-emacs assumes a Windows command shell, which you change
;; here.
;;
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 


;; Bind custom dired functions and fix search
(setq dired-load-hook
      (lambda (&rest ignore)
 (define-key dired-mode-map
   "l" 'dired-w32-browser)
 (define-key dired-mode-map
   "e" 'dired-w32explore)
 (define-key dired-mode-map
   "f" 'dired-show-only)))
(put 'dired-find-alternate-file 'disabled nil)


;; Set tramp-default-method 
(cond  ((eq window-system 'w32)
      (setq tramp-default-method "sshx"))
      (t
      (setq tramp-default-method "pscp")))

(require 'james-gui)

(provide 'james-windows)