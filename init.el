;; Speed up startup: suppress GC and file-name-handler overhead
(setq gc-cons-threshold most-positive-fixnum)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024))
    (setq file-name-handler-alist default-file-name-handler-alist)))

;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Turn off splash screens, etc.
(setq inhibit-startup-message t)
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
	   (load-file "~/.emacs.d/site-lisp/james-osx.el")))

;; Local initialization options can be saved in local.el
(setq local-init (concat user-emacs-directory "local.el"))
(when (file-exists-p local-init)
  (load local-init))

(require 'james-functions)



;; ================================
;; Behavior modifications
;; ================================

;; Leave lines at top or bottom when recentering
(setq scroll-margin 3)

;; Set tabs up for source files that already have tabs
(setq-default tab-width 4)

;; Set initial mode to text-mode
(setq-default initial-major-mode 'text-mode)

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; Kill whole line including newline when at beginning of line
(setq kill-whole-line t)

;; Delete files into trash
(setq delete-by-moving-to-trash t)

;;Prevent backup files from being made
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Limit VC to Git only (needed for diff-hl); magit handles interactive git
(setq vc-handled-backends '(Git))

;; Don't create lock files (.#filename) - slow on network/synced drives
(setq create-lockfiles nil)

;; Skip bidirectional text scanning (not editing RTL languages)
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Increase subprocess read buffer for better LSP/eglot throughput
(setq read-process-output-max (* 1024 1024))

;; Have typing get rid of the active selection
(delete-selection-mode t)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

;;; Make all yes-or-no questions as y-or-n
(setopt use-short-answers t)
(column-number-mode 1)

(setq ns-pop-up-frames nil)

(electric-pair-mode t)

;; Enable camel-case awareness in all programming modes
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)

(pixel-scroll-precision-mode t)
(which-key-mode t)
(global-so-long-mode 1)


;; ==============================
;; Built-in packages
;; ==============================

(use-package vertico
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles orderless basic))))
  (orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
		 ("C-x C-b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("<M-f10>" . consult-imenu)
         ("C-c o" . consult-line)
         ("C-x M-b" . consult-buffer-other-window)
         ("M-y" . consult-yank-from-kill-ring))
  :config
  (consult-customize
   consult-buffer consult-buffer-other-window
   :preview-key "M-."))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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
  :ensure nil)

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
  (when (display-graphic-p)
    (server-start)))

(add-hook 'js-mode-hook (lambda ()
                          (require 'js-mode-expansions)
                          (er/add-js-mode-expansions)))


;; ==============================
;; External packages
;; ==============================


(use-package mood-line
  :config
  (mood-line-mode))

(use-package diminish
  :hook (after-init . (lambda () (diminish 'subword-mode))))

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-arguments '("-l"))  ; login shell only, skip -i for speed
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

(use-package yaml-mode
  :defer t)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package treemacs
  :bind (("<f9>" . treemacs-toggle-current-project)
         ("C-x t t" . treemacs-toggle-current-project)
         ("C-x t 1" . treemacs-select-window))
  :commands (treemacs-toggle-current-project)
  :custom
  (treemacs-width 35)
  (treemacs-is-never-other-window t)
  :config
  (defun treemacs-toggle-current-project ()
    "Toggle treemacs. When opening, show the current project."
    (interactive)
    (if (treemacs-get-local-window)
        (treemacs)
      (treemacs-add-and-display-current-project-exclusively))))

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
  :bind-keymap ("C-c C" . claude-code-command-map))

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
  (global-corfu-modes '(not org-mode))
  :config
  (global-corfu-mode))



;; ======================
;; Org mode
;; ======================


(setq org-directory "~/OneDrive - Raytheon Technologies/org")
(setq org-agenda-files '("~/OneDrive - Raytheon Technologies/org"))
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "OBE")))
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(600))
(add-hook 'org-mode-hook 'auto-save-visited-mode)
(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2)))

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d%H%M%S-"
        org-download-screenshot-method "screencapture -i %s"
        org-download-annotate-function (lambda (_link) ""))
  ;; Enable drag-and-drop on macOS
  (setq dnd-protocol-alist
        '(("^file:" . org-download-dnd)
          ("^http" . org-download-dnd)))
  :hook (org-mode . org-download-enable))


(defun my/org-return ()
  "In a checkbox list item, RET creates a new checkbox item.
On an empty checkbox item, remove it and insert a newline.
Otherwise, normal return."
  (interactive)
  (if (and (org-in-item-p)
           (save-excursion
             (beginning-of-line)
             (looking-at "\\s-*- \\[.\\]")))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*- \\[.\\]\\s-*$"))
          ;; Empty checkbox item — remove it and exit
          (progn
            (delete-region (line-beginning-position) (line-end-position))
            (delete-char -1)  ; remove the trailing newline
            (org-return))
        ;; Non-empty checkbox item — create a new one
        (org-insert-item 'checkbox))
    (org-return)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") #'my/org-return))


(defun my/org-sort-checkboxes ()
  "Sort checkbox list, unchecked first."
  (interactive)
  (org-sort-list nil ?f
    (lambda ()
      (if (looking-at ".*\\[X\\]") 1 0))
    #'<))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
    '("w" "Waiting/Owed to me"
      todo "WAITING"
      ((org-agenda-sorting-strategy '(deadline-up scheduled-up))))))

;; =======================
;; Key bindings
;; =======================

;; Rip grep
(global-set-key (kbd "C-c r") #'consult-ripgrep)

;; Org mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)


;; Line editing
(bind-keys ("<C-return>" . open-line-below)
           ("<C-S-return>" . open-line-above)
           ("C-c y" . djcb-duplicate-line)
           ("C-x C-j" . join-line)
           ("C-;" . comment-dwim-line)
           ("C-c ;" . comment-dwim-line)
           ("C-c d" . delete-enclosed-text))

;; Navigation
(bind-keys ("C-." . xref-go-back)
           ("<home>" . smart-beginning-of-line)
           ("C-a" . smart-beginning-of-line)
           ("C-c [" . beginning-of-defun)
           ("C-c ]" . end-of-defun)
           ("C-c g" . goto-line)
           ("C-x C-m" . execute-extended-command))

;; Kill, copy, undo
(bind-keys ("C-z" . undo)
		   ("H-v" . yank)
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

;; Shell / dired
(bind-keys ("C-c e" . eat)
           ("C-c j" . dired-jump)
           ("C-c s" . shell-command))

;; Function keys
(bind-keys ("<f4>" . call-last-kbd-macro)
           ("<f5>" . revert-buffer)
           ("<C-f5>" . revert-buffer-no-confirm)
           ("<f6>" . swap-windows)
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
