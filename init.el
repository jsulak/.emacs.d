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

(unless package-archive-contents
  (package-refresh-contents))

;; use-package is built into Emacs 29+
(require 'use-package)
(setq use-package-always-ensure t)

;; Automatically refresh archives if a package install fails
(defvar james--package-refreshed nil)
(advice-add 'package-install :around
            (lambda (orig-fun &rest args)
              (condition-case nil
                  (apply orig-fun args)
                (error
                 (unless james--package-refreshed
                   (setq james--package-refreshed t)
                   (package-refresh-contents)
                   (apply orig-fun args))))))


;; =======================
;; Load path
;; =======================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
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


(use-package solarized-theme)



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

(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;;  electric layout doesn't work right with js-mode
(defun james-js-mode-hook ()
  (electric-layout-mode -1))
(add-hook 'js-mode-hook 'james-js-mode-hook)

;; Enable camel-case awareness in all programming modes
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)

(pixel-scroll-precision-mode t)

;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)


;; ==============================
;; Built-in packages
;; ==============================

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("<M-f10>" . consult-imenu)
         ("C-c o" . consult-line)
         ("C-x M-b" . consult-buffer-other-window))
  :config
  (consult-customize
   consult-buffer consult-buffer-other-window
   :preview-key "M-."))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 25))

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

(use-package project
  :ensure nil
  :bind (("C-x p f" . project-find-file)
         ("C-x p g" . project-find-regexp)
         ("C-x p c" . project-compile)
         ("C-x p d" . project-dired)))

(use-package dired
  :ensure nil
  :custom
  (dired-isearch-filenames t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

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

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package diminish
  :config
  (diminish 'subword-mode))

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package move-text
  :bind (("<C-S-down>" . move-text-down)
         ("<C-S-up>" . move-text-up)))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode)

(use-package magit
  :bind ("C-x g" . magit-status))

(defun treemacs-toggle-current-project ()
  "Toggle treemacs. When opening, show the current project."
  (interactive)
  (if (treemacs-get-local-window)
      (treemacs)
    (treemacs-add-and-display-current-project-exclusively)))

(use-package treemacs
  :bind (("<f9>" . treemacs-toggle-current-project)
         ("C-x t t" . treemacs-toggle-current-project)
         ("C-x t 1" . treemacs-select-window))
  :custom
  (treemacs-width 35)
  (treemacs-is-never-other-window t))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist '(typescript tsx))
  (global-treesit-auto-mode))

(use-package eat
  :custom
  (eat-term-name "xterm-256color")
  :bind ("C-x t e" . eat-project))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (css-mode . eglot-ensure)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode))


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

;; Line editing
(bind-keys ("<C-return>" . open-line-below)
           ("<C-S-return>" . open-line-above)
           ("C-c y" . djcb-duplicate-line)
           ("C-x C-j" . join-line)
           ("C-;" . comment-dwim-line)
           ("C-c ;" . comment-dwim-line)
           ("C-c d" . delete-enclosed-text))

;; Duplicate line and comment the original
(global-set-key (kbd "C-c c") (lambda () (interactive) (djcb-duplicate-line t)))

;; Navigation
(bind-keys ("M-." . xref-find-definitions)
           ("C-." . xref-go-back)
           ("<home>" . smart-beginning-of-line)
           ("C-a" . smart-beginning-of-line)
           ("C-c [" . beginning-of-defun)
           ("C-c ]" . end-of-defun)
           ("C-c g" . goto-line)
           ("C-x C-m" . execute-extended-command))

;; Kill, copy, undo
(bind-keys ("C-z" . undo)
           ("C-w" . kill-word)
           ("C-x C-k" . kill-region)
           ("C-c C-k" . kill-region)
           ("C-q" . backward-kill-word)
           ("C-c C-q" . quoted-insert)
           ("M-z" . zap-up-to-char)
           ("C-c z" . zap-up-to-char)
           ("C-x k" . jcs-kill-a-buffer))

;; Completion
(bind-keys ("M-;" . hippie-expand)
           ("C-=" . hippie-expand))

;; Shell / eshell
(bind-keys ("C-c e" . eshell)
           ("C-c j" . dired-jump)
           ("C-c s" . shell-command))
(global-set-key (kbd "C-c E") (lambda () (interactive) (eshell t)))
(when (eq system-type 'darwin)
  (global-set-key (kbd "C-c l") 'open-current-buffer-mac))

;; Function keys
(bind-keys ("<f4>" . call-last-kbd-macro)
           ("<f5>" . revert-buffer)
           ("<C-f5>" . revert-buffer-no-confirm)
           ("<f6>" . swap-windows)
           ("<f7>" . vc-diff)
           ("<f8>" . indent-region))

;; Windows
(bind-keys ("S-<left>" . windmove-left)
           ("S-<right>" . windmove-right)
           ("S-<up>" . windmove-up)
           ("S-<down>" . windmove-down)
           ("C-x 5" . xsteve-split-window))

;; Font size
(bind-keys ("C-+" . ryan/increase-font-size)
           ("C--" . ryan/decrease-font-size))
