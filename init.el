;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off splash screens, etc.
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)


;; ==============================
;; Package management
;; ==============================

(require 'package)


;; Define package repositories
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives melpa-stable t)

(unless (and 	 (file-exists-p "~/.emacs.d/elpa/archives/gnu")
		 (file-exists-p "~/.emacs.d/elpa/archives/melpa")
		 (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (mapc (lambda (package)
		  (let ((name (car package))
				(repo (cdr package)))
			(when (not (package-installed-p name))
			  (let ((package-archives (list repo)))
				(package-initialize)
				(package-install name)))))
		packages)
  (package-initialize)
  (delete-other-windows))

;; List of all packages to download / load from repositories
(defun init--install-packages ()
  (packages-install   
   (cons 'browse-kill-ring melpa)
   (cons 'diminish melpa)
   (cons 'expand-region melpa)
   (cons 'exec-path-from-shell melpa)
   (cons 'move-text melpa)
   (cons 'markdown-mode melpa)
   (cons 'smex melpa)
   (cons 'undo-tree gnu)
   (cons 'rainbow-mode gnu)
   (cons 'which-key melpa)
   (cons 'yaml-mode melpa)))

(condition-case nil
	(init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;; =======================
;; Load path
;; =======================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/external"))

;; Environment-specific configurations
(cond ((or (eq system-type 'gnu/linux)
	  (eq system-type 'linux))
	   (load-file "~/.emacs.d/site-lisp/james-linux.el"))
	  ((eq system-type 'darwin)
	   (load-file "~/.emacs.d/site-lisp/james-osx.el"))
	  ((eq system-type 'windows-nt)
	   (load-file "~/.emacs.d/site-lisp/james-windows.el")))

;; Local initialization options can be saved in local.el
(setq local-init (concat user-emacs-directory "local.el"))
(when (file-exists-p local-init)
  (load local-init))

(require 'james-functions)

(add-to-list 'custom-theme-load-path "~/.emacs.d/external/solarized")

;(require 'spaceline-config)
;(spaceline-emacs-theme)


;; ================================
;; Behavior modifications
;; ================================

;; Leave lines at top or bottom when recentering
(setq scroll-margin 3)

;; Set tabs up for source files that already have tabs
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; Set initial mode to text-mode
(setq-default initial-major-mode 'text-mode)

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; Delete files into trash
(setq delete-by-moving-to-trash t)

;;Prevent backup files from being made
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Have typing get rid of the active selection
(delete-selection-mode t)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)


;;http://www.xsteve.at/prg/emacs/power-user-tips.html
(setq recentf-max-saved-items 500)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)



;; ==============================
;; Ido mode
;; ==============================

;; Ido setup
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-enable-prefix nil
	  ido-enable-flex-matching t
	  ido-auto-merge-work-directories-length nil
	  ido-create-new-buffer 'always
	  ido-use-filename-at-point nil
	  ido-use-virtual-buffers t
	  ido-handle-duplicate-virtual-buffers 2
	  ido-max-prospects 15)


;; ==============================
;; Activate various modes
;; ==============================

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Activate winner mode
(when (fboundp 'winner-mode)
	  (winner-mode 1))


;; uniquify - make buffer names more unique
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-seperator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

(require 'etags)
(setq tags-revert-without-query 1)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Multiple cursors
(require 'sgml-mode)
(define-key sgml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair)


;; (electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;;  electric layout doesn't work right with js-mode
(defun james-js-mode-hook ()
  (electric-layout-mode -1))
(add-hook 'js-mode-hook 'james-js-mode-hook)

(add-hook 'js-mode-hook (lambda ()
						  (require 'js-mode-expansions)
						  (er/add-js-mode-expansions)))
(setq pending-delete-mode t)



;; Add more file types to find-file-in-project
(defvar ffip-patterns
  '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl"
	"*.sh" "*.erl" "*.hs" "*.ml" "*.py" "*.xslt" "*.xsl" "*.xpl" "*.cs" "*.zsh"
	"*.erb" "*.coffee" "*.xml" "*.xqy" "*.xqm")
  "List of patterns to look for with `find-file-in-project'.")

(setq ns-pop-up-frames nil)

;; Dired mode
;; Make dired mode search filenames only
(setq dired-isearch-filenames t)
;; Make dired open directories in same buffer
(put 'dired-find-alternate-file 'disabled nil)
(autoload 'dired-jump "dired-x" "Jump to dired corresponding current buffer.")
(autoload 'dired-jump-other-window "dired-x" "jump to dired in other window.")


;; Enable camel-case awareness in all programming modes
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)

;; ========================
;; Major modes
;; ========================


;; Add support for scss to css mode
(setq auto-mode-alist
	  (cons '("\\.\\(scss\\)\\'" . css-mode)
			auto-mode-alist))
;; Automatically load rainbow mode in css mode
(add-hook 'css-mode-hook 'rainbow-mode)

;; Add markdown mode
(setq auto-mode-alist
	  (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions
		  'comint-strip-ctrl-m)


;; =======================
;; Server
;; =======================

(require 'server)
(when (equal window-system 'w32)
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
												 ; ~/.emacs.d/server is unsafe"
												 ; on windows.
(server-start)


;; ======================
;; Diminish (clean up) mode line
;; ======================

(diminish 'smartparens-mode)
(diminish 'undo-tree-mode)


;; =======================
;; Key bindings
;; =======================

(require 'james-bindings)

(which-key-mode)


;; =======================
;; Smex.  Must be at end of .emacs
;; =======================

(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
