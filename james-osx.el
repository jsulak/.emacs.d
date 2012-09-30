(menu-bar-mode 1)

(setq ring-bell-function 'ignore)
(menu-bar-mode 1)

(if window-system
    (require 'james-gui))

(if window-system
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))

;; (setq load-path (append load-path '("~/.emacs.d/themes/solarized-theme")))
;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)
 
(add-to-list 'custom-theme-load-path "~/.emacs.d/external/solarized")
(load-theme 'solarized-dark)

;;(load-file "~/.emacs.d/external/color-theme-tangotango.el")
;;(color-theme-tangotango)

;; (set-face-attribute 'default nil :font "Menlo-11") 

;; (set-face-attribute 'default nil :font "Inconsolata-13")
;; (set-face-attribute 'default nil :font "Letter Gothic-14")

;; Setup path to work.
;; From http://blog.dskang.com/2011/04/28/emacs-path-problem-on-os-x/

;; Apply shell environment to emacs
;; http://paste.lisp.org/display/111574

(defun env-line-to-cons (env-line)
  "convert a string of the form \"VAR=VAL\" to a 
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
    (cons (match-string 1 env-line) (match-string 2 env-line))))

(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "launch /usr/bin/env or the equivalent from an interactive
shell, parsing and returning the environment as an alist."
  (let ((cmd (concat (or shell-cmd "/bin/bash -ic")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (or (not (string-match "=" str))
	       (string-equal str "")))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))

(defun setenv-from-cons (var-val)
  "set an environment variable from a cons cell containing 
two strings, where the car is the variable name and cdr is 
the value, e.g. (\"VAR\" . \"VAL\")"
  (setenv (car var-val) (cdr var-val)))

(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
 "apply the environment reported by `/usr/bin/env' (or env-cmd) 
as launched by `/bin/bash -ic' (or shell-cmd) to the current 
environment."
 (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))

(setenv-from-shell-environment)
(setq exec-path (split-string (getenv "PATH") path-separator))

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


;; Set up rsense
;; (setq rsense-home (expand-file-name "~/opt/rsense-0.3"))
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (setq rsense-rurema-home "~/src/rurema")
;; (require 'rsense)

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;; ;; Complete by C-c .
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c .") 'ac-complete-rsense)))

(require 'tramp)

;; JS hint mode
;; (add-to-list  'load-path "/usr/local/lib/node_modules/jshint-mode")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/external/jshint-mode"))
(require 'flymake-jshint)
(add-hook 'js-mode-hook
         (lambda () (flymake-mode t)))
(require 'flymake-cursor)
