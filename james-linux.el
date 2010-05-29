
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#162433" :foreground "#C7D4E2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 72 :width normal :foundry "microsoft" :family "Consolas"))))
 '(cperl-array-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(cperl-hash-face ((((class color) (background dark)) (:slant italic :weight bold))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "orange"))))
 '(js2-jsdoc-html-tag-name-face ((((class color) (min-colors 8) (background dark)) nil)))
 '(linum ((t (:inherit (shadow default) :foreground "darkgray")))))

;; Bind custom dired functions and fix search
(setq dired-load-hook
      (lambda (&rest ignore)
 (define-key dired-mode-map
   "l" 'dired-launch-command)
 (define-key dired-mode-map
   "f" 'dired-show-only)))
(put 'dired-find-alternate-file 'disabled nil)

(server-start)

;(add-hook 'kill-emacs-hook 
;	  (lambda ()
;	    (setq s-name (concat server-socket-dir "/server"))
;	    (when (file-exists-p s-name)
;	      (delete-file server-name))))

(provide 'james-linux)