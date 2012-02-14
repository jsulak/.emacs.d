(menu-bar-mode 0)

;;Set the default font
(setq default-frame-alist
      '((font .
"-outline-Consolas-normal-r-normal-normal-11-97-96-96-c-*-iso8859-1")
        (vertical-scroll-bars . right)))


(require 'w32-browser)

;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).
(setq exec-path (cons "C:/cygwin/bin" exec-path))
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
	ad-do-it))
(ad-activate 'grep-compute-defaults)

;; NT-emacs assumes a Windows command shell, which you change
;; here.
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name)
(setq ediff-shell               shell-file-name) 
(setq explicit-sh-args '("--login" "-i"))


;; Set up cygwin-mount
(require 'cygwin-mount)
(cygwin-mount-activate)

;; Set python executable path;
;; (setq python-python-command "C:\\python27\\python.exe") ::)

;;  ;\;\pdb setup, note the python version ::)
;;  (setq pdb-path 'c:/python27/lib/pdb.py ::)
;;        gud-pdb-command-name (symbol-name pdb-path)) ::)
;;  (defadvice pdb (before gud-query-cmdline activate) ::)
;;    "Provide a better default command line when called interactively." ::)
;;    (interactive ::)
;;     (list (gud-query-cmdline pdb-path ::)
;; 	 		    (file-name-nondirectory buffer-file-name))))) ::)


;;; Add Cygwin Info pages
(setq Info-default-directory-list (append Info-default-directory-list (list "c:/cygwin/usr/info/")))


;; Bind custom dired functions and fix search
(setq w32-browser-wait-time 1)
(setq dired-load-hook
      (lambda (&rest ignore)
 (define-key dired-mode-map
   "l" 'dired-w32-browser)
 (define-key dired-mode-map
   "e" 'dired-w32explore)
 (define-key dired-mode-map
   "f" 'dired-show-only)))
(put 'dired-find-alternate-file 'disabled nil)


(require 'tramp)
(setq tramp-default-method "plink")

(require 'james-gui)


;; (load-file "~/.emacs.d/external/color-theme-tangotango.el")
;; (color-theme-tangotango)
;; (set-face-background 'hl-line "#2B3030")
;; 

;; (setq load-path (append load-path '("~/.emacs.d/themes/solarized-theme")))
;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)

(add-to-list 'custom-theme-load-path "~/.emacs.d/external/solarized")
(load-theme 'solarized-dark)

;; (load-file "~/.emacs.d/external/ruby-blue-theme.el")
;; (color-theme-ruby-blue)
;; (set-face-background 'hl-line "#19293A")



(provide 'james-windows)
