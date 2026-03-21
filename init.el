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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))

;; use-package is built into Emacs 29+
(require 'use-package)
(setq use-package-always-ensure t)


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
(setopt use-short-answers t)
(column-number-mode 1)

(setq ns-pop-up-frames nil)

;; (electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;;  electric layout doesn't work right with js-mode
(defun james-js-mode-hook ()
  (electric-layout-mode -1))
(add-hook 'js-mode-hook 'james-js-mode-hook)

;; Enable camel-case awareness in all programming modes
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)

;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)


;; ==============================
;; Built-in packages
;; ==============================

(use-package ido
  :ensure nil
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  :custom
  (ido-enable-prefix nil)
  (ido-enable-flex-matching t)
  (ido-auto-merge-work-directories-length nil)
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point nil)
  (ido-use-virtual-buffers t)
  (ido-handle-duplicate-virtual-buffers 2)
  (ido-max-prospects 15))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 25))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-shrink-to-minimum-size t)
  (ibuffer-always-show-last-buffer nil)
  (ibuffer-sorting-mode 'recency)
  (ibuffer-use-header-line t))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator ":")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package dired
  :ensure nil
  :custom
  (dired-isearch-filenames t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package server
  :ensure nil
  :config
  (when (equal window-system 'w32)
    (defun server-ensure-safe-dir (dir) "Noop" t))
  (server-start))

(add-hook 'js-mode-hook (lambda ()
                          (require 'js-mode-expansions)
                          (er/add-js-mode-expansions)))


;; ==============================
;; External packages
;; ==============================

(use-package amx
  :bind (("M-x" . amx)
         ("C-x C-m" . amx)
         ("C-x m" . amx)
         ("M-X" . amx-major-mode-commands))
  :config
  (amx-mode 1))

(use-package browse-kill-ring)

(use-package diminish
  :config
  (diminish 'subword-mode))

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package exec-path-from-shell)

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package move-text)

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode)

;; Multiple cursors (requires multiple-cursors package)
;; (use-package multiple-cursors
;;   :bind (("C-<" . mc/mark-previous-like-this)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-*" . mc/mark-all-like-this-dwim)
;;          ("C-S-c C-S-c" . mc/edit-lines)
;;          ("C-S-c C-e" . mc/edit-ends-of-lines)
;;          ("C-S-c C-a" . mc/edit-beginnings-of-lines)
;;          ("C-c SPC" . set-rectangular-region-anchor)
;;          ("C-c C-SPC" . set-rectangular-region-anchor)))


;; ========================
;; Major modes
;; ========================

;; Add support for scss to css mode
(setq auto-mode-alist
	  (cons '("\\.\\(scss\\)\\'" . css-mode)
			auto-mode-alist))


;; =======================
;; Key bindings
;; =======================

(require 'james-bindings)
