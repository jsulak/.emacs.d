;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

(defconst james/config-directory
  (file-name-directory (or load-file-name user-init-file))
  "Directory containing this Emacs configuration.")

(defvar james/load-custom-config t
  "When non-nil, load the Customize-generated configuration file.")

(defvar james/load-local-config t
  "When non-nil, load machine-local configuration overrides.")

;; Speed up startup: suppress GC and file-name-handler overhead
(setq gc-cons-threshold most-positive-fixnum)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024))
    (setq file-name-handler-alist default-file-name-handler-alist)))

;; Turn off splash screens, etc.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

 
;; ==============================
;; Package management
;; ==============================

(require 'package)
(require 'bookmark)

(declare-function consult--customize-put "consult")
(declare-function er/add-js-mode-expansions "js-mode-expansions")
(declare-function global-treesit-auto-mode "treesit-auto")
(declare-function treesit-auto-add-to-auto-mode-alist "treesit-auto")
(declare-function posframe-delete-frame "posframe")
(declare-function server-running-p "server")
(declare-function server-start "server")
(declare-function treemacs-get-local-window "treemacs")
(declare-function james/vertico-posframe-reset-after-frame-move "init")
(declare-function james/vertico-posframe-update-border-face "init")
(declare-function james/dired-open-mac "james-osx")

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

(defun james/package-install-refresh-once (orig-fun &rest args)
  "Refresh package archives once before retrying ORIG-FUN with ARGS."
  (condition-case err
      (apply orig-fun args)
    (error
     (if james--package-refreshed
         (signal (car err) (cdr err))
       (setq james--package-refreshed t)
       (package-refresh-contents)
       (apply orig-fun args)))))

(advice-add 'package-install :around #'james/package-install-refresh-once)


;; =======================
;; Load path
;; =======================

(setq custom-file (expand-file-name "custom.el" james/config-directory))
(when james/load-custom-config
  (load custom-file 'noerror))

(add-to-list 'load-path (expand-file-name "site-lisp" james/config-directory))
;; Environment-specific configurations
(cond ((or (eq system-type 'gnu/linux)
	  (eq system-type 'linux))
	   (load-file (expand-file-name "site-lisp/james-linux.el"
	                                james/config-directory)))
	  ((eq system-type 'darwin)
	   (load-file (expand-file-name "site-lisp/james-osx.el"
	                                james/config-directory))))

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

;; Auto-focus help windows when they open
(setq help-window-select t)

;; Save bookmarks after every change (no loss on crash)
(setq bookmark-save-flag 1)

;; Allow repeating certain key sequences (e.g. C-x o o o to switch windows)
(repeat-mode 1)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

;;; Make all yes-or-no questions as y-or-n
(setopt use-short-answers t)
(column-number-mode 1)

(electric-pair-mode t)

;; Enable camel-case awareness in all programming modes
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)

(pixel-scroll-precision-mode t)
(which-key-mode t)
(global-so-long-mode 1)

(setq-default cursor-type 'bar)


;; ==============================
;; Built-in packages
;; ==============================

(use-package vertico
  :init
  (vertico-mode))

(use-package vertico-posframe
  :after vertico
  :if (display-graphic-p)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
  (vertico-posframe-width 100)
  (vertico-posframe-min-width 100)
  (vertico-posframe-border-width 1)
  :config
  (defun james/vertico-posframe-update-border-face (&rest _)
    "Set the Vertico posframe border color from the active theme."
    (let ((color (or (face-foreground 'border nil t)
                     (face-foreground 'vertical-border nil t)
                     (face-foreground 'shadow nil t)
                     "gray70")))
      (set-face-background 'vertico-posframe-border color)))

  (james/vertico-posframe-update-border-face)
  (add-hook 'after-load-theme-functions
            #'james/vertico-posframe-update-border-face)

  (defun james/vertico-posframe-reset-after-frame-move (frame)
    "Recreate Vertico's posframe after its parent FRAME moves."
    (when (and (frame-live-p frame)
               (not (frame-parameter frame 'parent-frame))
               (buffer-live-p vertico-posframe--buffer))
      (posframe-delete-frame vertico-posframe--buffer)))

  ;; Posframe caches relative coordinates, which can leave its native child
  ;; frame at the old screen position after the parent is moved on macOS.
  (add-hook 'move-frame-functions
            #'james/vertico-posframe-reset-after-frame-move)
  (vertico-posframe-mode 1))

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
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (dolist (category '(file project-file buffer project-buffer))
    (setf (alist-get category marginalia-annotators) '(none))))

(use-package consult
  :bind (("C-x b" . consult-buffer)
		 ("C-x C-b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("<M-f10>" . consult-imenu)
         ("C-c o" . consult-line)
         ("C-x M-b" . consult-buffer-other-window)
		 ("C-c b" . consult-bookmark)
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

(use-package jinx
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)
         :map org-mode-map
         ("C-;" . jinx-correct)))
(setq jinx-languages "en_US")


(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         :map isearch-mode-map
         ("M-j" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.3))

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
(setq project-vc-extra-root-markers '(".project"))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("f" . james/dired-show-only))
  :custom
  (dired-isearch-filenames t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (when (eq system-type 'darwin)
    (keymap-set dired-mode-map "l" #'james/dired-open-mac)))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

(use-package server
  :ensure nil
  :config
  (when (and (or (display-graphic-p)
                 (eq system-type 'gnu/linux))
             (not (server-running-p)))
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
  :mode "\\.md\\'"
  :bind (:map markdown-mode-map
              ("C-c z b" . james/zk-backlinks)
              ("C-c z l" . james/zk-insert-link))
  :config
  (require 'james-markdown))

(use-package move-text
  :bind (("<C-S-down>" . move-text-down)
         ("<C-S-up>" . move-text-up)))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package vundo
  :bind ("C-x u" . vundo))

;; Kitty keyboard protocol — richer key reporting in capable terminals
;; (distinguishes C-i from TAB, C-m from RET, supports all modifier combos).
;; global-kkp-mode only activates on TTY frames; GUI frames are unaffected.
(use-package kkp
  :config
  (global-kkp-mode 1))

(use-package yaml-mode
  :defer t)

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-bury-buffer-function #'magit-restore-window-configuration))

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



(require 'james-org)

(declare-function org-download-clipboard "org-download")
(declare-function org-download-screenshot "org-download")

;; =======================
;; Key bindings
;; =======================

;; Rip grep
(global-set-key (kbd "C-c r") #'consult-ripgrep)


;; Line editing
(bind-keys ("<C-return>" . james/open-line-below)
           ("<C-S-return>" . james/open-line-above)
           ("C-h" . backward-delete-char-untabify)
           ("C-c y" . james/duplicate-line)
           ("C-x C-j" . join-line)
           ("C-;" . james/comment-dwim-line)
           ("C-c ;" . james/comment-dwim-line)
           ("C-c d" . james/delete-enclosed-text))

;; Navigation
(bind-keys ("C-." . xref-go-back)
           ("<home>" . james/smart-beginning-of-line)
           ("C-a" . james/smart-beginning-of-line)
           ("C-c [" . beginning-of-defun)
           ("C-c ]" . end-of-defun)
           ("C-c g" . goto-line)
           ("C-x C-m" . execute-extended-command))
(bind-keys :map isearch-mode-map
           ("C-o" . james/isearch-occur))

;; Kill, copy, undo
(defun james/kill-buffer-quick ()
  "Kill current buffer immediately if unmodified, otherwise prompt."
  (interactive)
  (if (buffer-modified-p)
      (kill-buffer (current-buffer))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(bind-keys ("C-z" . undo)
		   ("H-v" . yank)
           ("C-w" . kill-word)
           ("C-x C-k" . kill-region)
           ("C-x k" . james/kill-buffer-quick)
           ("C-c C-k" . kill-region)
           ("C-q" . backward-kill-word)
           ("C-c C-q" . quoted-insert)
           ("M-z" . zap-up-to-char)
           ("C-c z" . zap-up-to-char)
)

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
           ("<C-f5>" . james/revert-buffer-no-confirm)
           ("<f6>" . james/swap-windows)
           ("<f8>" . indent-region))

;; Windows
(bind-keys ("S-<left>" . windmove-left)
           ("S-<right>" . windmove-right)
           ("S-<up>" . windmove-up)
           ("S-<down>" . windmove-down)
           ("C-x 5" . james/split-window))

(advice-add 'split-window-below :after
            (lambda (&rest _) (other-window 1)))

;; Font size
(bind-keys ("C-+" . james/increase-font-size)
           ("C--" . james/decrease-font-size))

;; Conversion
(bind-keys ("C-c m" . james/paste-markdown-as-org))

;; Org mode
(bind-keys ("C-c a" . org-agenda)
           ("C-c c" . org-capture)
           ("C-c l" . org-store-link)
           ("C-c f" . james/org-insert-file-link)
           ("C-c p" . james/org-agenda-person-view)
           :map org-mode-map
           ("C-c b" . james/org-emphasize-bold)
           ("C-c i" . james/org-emphasize-italic)
           ("C-c u" . james/org-emphasize-underline)
           ("C-c s" . james/org-sort-checkboxes)
           ("C-c v a" . james/org-attachment-insert)
           ("C-c v s" . org-download-screenshot)
           ("C-c v y" . org-download-clipboard)
           ("C-c w" . ox-clip-formatted-copy))
(define-key org-mode-map [double-mouse-1] #'james/org-open-inline-image)

;; Local machine-specific overrides (loaded last so they take precedence)
(let ((local-init (expand-file-name "local.el" james/config-directory)))
  (when (and james/load-local-config
             (file-exists-p local-init))
    (load local-init)))
