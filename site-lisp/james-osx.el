(menu-bar-mode 1)

(setq ring-bell-function 'ignore)

(if window-system
    (require 'james-gui))

;; (if window-system
	 ;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))

;; (setq load-path (append load-path '("~/.emacs.d/themes/solarized-theme")))
;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)
 
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/external/solarized")
;; (load-theme 'solarized-dark)

;;(load-file "~/.emacs.d/external/color-theme-tangotango.el")
;;(color-theme-tangotango)

;; (set-face-attribute 'default nil :font "Menlo-11") 

;; (set-face-attribute 'default nil :font "Inconsolata-13")
;; (set-face-attribute 'default nil :font "Letter Gothic-14")

(exec-path-from-shell-initialize)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; Dired mode
(setq dired-load-hook
      (lambda (&rest ignore)
 (define-key dired-mode-map
   "l" 'dired-open-mac)
 (define-key dired-mode-map
   "f" 'dired-show-only)))
(put 'dired-find-alternate-file 'disabled nil)


(defun open-file-mac (file-name)
  (if (file-exists-p file-name)
      (call-process "/usr/bin/open" nil 0 nil file-name)))

(defun dired-open-mac ()
     (interactive)
     (let ((file-name (dired-get-file-for-visit)))
     (open-file-mac file-name)))
 
(defun open-current-buffer-mac ()
  (interactive)
  (open-file-mac buffer-file-name))


(require 'tramp)
(setenv "TMPDIR" "/tmp")

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

