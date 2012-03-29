;; =======================
;; Package.el
;; =======================
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(bm csharp-mode cygwin-mount find-file-in-project ido-ubiquitous
                         magit markdown-mode ruby-end smex sml-modeline undo-tree yaml-mode yasnippet-bundle)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; =======================
;; Load path
;; =======================

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/external"))
;; (require 'bytecomp)
;; (byte-recompile-directory "~/.emacs.d/external" 0)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(cond ((or (eq system-type 'gnu/linux)
	  (eq system-type 'linux))
       (load-file "~/.emacs.d/james-linux.el"))
      ((and (eq system-type 'darwin)
            (window-system)
            (load-file "~/.emacs.d/james-osx.el")))
      ((eq system-type 'windows-nt)
       (load-file "~/.emacs.d/james-windows.el")))

;; Local initialization options can be saved in local.el
(setq local-init (concat user-emacs-directory "local.el"))
(when (file-exists-p local-init)
  (load local-init))

(require 'james-functions)

;; ================================
;; Behavior
;; ================================

;; Get rid of scroll bars and use sml-modeline instead, and make the fringe half-width
(sml-modeline-mode 1)
(scroll-bar-mode -1)
(set-fringe-style 5)

(require 'linum)

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

;; (electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;;  electric layout doesn't work right with js-mode
(defun james-js-mode-hook ()
  ;; electric-layout-mode doesn't play nice with js-mode
  (electric-layout-mode -1))
(add-hook 'js-mode-hook 'james-js-mode-hook)


;; Add more file types to find-file-in-project
(defvar ffip-patterns
  '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl"
    "*.sh" "*.erl" "*.hs" "*.ml" "*.py" "*.xslt" "*.xsl" "*.xpl" "*.cs" "*.zsh"
    "*.erb" "*.coffee" "*.xml" "*.acl" "*.bat" "*.cmd" "*.xqy" "*.xqm")
  "List of patterns to look for with `find-file-in-project'.")

(setq ns-pop-up-frames nil)

;; Leave lines at top or bottom when recentering
(setq scroll-margin 3)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set initial mode to text-mode
(setq-default initial-major-mode 'text-mode)

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;;paren highlighting
(show-paren-mode 1)

;; Delete files into trash
(setq delete-by-moving-to-trash t)

;; Have typing get rid of the active selection
(delete-selection-mode t)

;; Make dired mode search filenames only
(setq dired-isearch-filenames t)
;; Make dired open directories in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;;http://www.xsteve.at/prg/emacs/power-user-tips.html
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
	   (line-end-position)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-end-position)))))

;; Activate winner mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;;Prevent backup files from being made
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Send deletions to Recycling Bin
(setq delete-by-moving-to-trash t)

;; uniquify - make buffer names more unique
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-seperator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;; point stack - forward/back stack for point
(require 'point-stack)

;; Adds extra keybinding to interactive search that sends the current term to occur
;; From http://emacsblog.org/page/5/
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))


;; ========================
;; Major modes 
;; ========================

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Coffee mode
(require 'coffee-mode)

;; Clojure
;; (require 'paredit) if you didn't install via package.el
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

(add-to-list 'load-path "~/.emacs.d/external/rinari")
(require 'rinari)

(require 'ruby-end)

;; nxml mode
(setq rng-schema-locating-files (quote ("schemas.xml" "~/.emacs.d/nxml-mode/schema/schemas.xml")))
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|xslt\\|rng\\|xhtml\\|xpr\\|xspec\\|xpl\\)\\'" . nxml-mode)
	      auto-mode-alist))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;Add support for dos batch files
(require 'dosbat)
(setq auto-mode-alist
        (cons '("\\.\\(bat\\|cmd\\)\\'" . bat-mode)
	      auto-mode-alist))

;; Add support for scss to css mode
(setq auto-mode-alist
      (cons '("\\.\\(scss\\)\\'" . css-mode)
            auto-mode-alist))

;;Add support for xquery-mode
(require 'xquery-mode)
(setq auto-mode-alist
      (cons '("\\.\\(xqy\\|xquery\\|xq\\|xqm\\)\\'" . xquery-mode)
	    auto-mode-alist))
(setq xquery-indent-size 4)

;;Add ACL mode mode 
(autoload 'acl-mode "acl-mode")
(setq auto-mode-alist (append (list (cons "\\.acl\\'" 'acl-mode))
                               auto-mode-alist))

;; Add markdown mode
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(add-to-list 'load-path "~/.emacs.d/external/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" .sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" .sws-mode))


;; =========================
;; External packages
;; =========================

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/external/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/external/autocomplete//ac-dict")
(setq ac-auto-start 3)
(setq ac-ignore-case nil)

;; Attempt to fix css autocomplete silliness
(define-key ac-complete-mode-map "\r" nil)


;; This turns off filename completion everywhere because it crashes in js.
;; It would be better to do it for js2-mode only
;; (defun ac-common-setup ())  
(ac-config-default)

(ac-set-trigger-key "TAB")

(require 'etags)
(require 'cl)
(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/text-mode")
(add-to-list 'ac-sources 'ac-source-yasnippet)
;; (eval-after-load 'js2-mode
;;   '(progn
;;      (define-key js2-mode-map (kbd "TAB") (lambda()
;;                                             (interactive)
;;                                             (let ((yas/fallback-behavior 'return-nil))
;;                                               (unless (yas/expand)
;;                                                 (indent-for-tab-command)
;;                                                 (if (looking-back "^\s*")
;;                                                     (back-to-indentation))))))))


(require 'grep-buffers)

;; Visible bookmakrs
(setq bm-restore-repository-on-load t)
(require 'bm)

(setq bm-highlight-style (quote bm-highlight-only-style))

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)
 
;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)
 
;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)
 
;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)
 
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;; Bookmark customizations
(setq 
  bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
  bookmark-save-flag 1)                        ;; autosave each change)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; =======================
;; Server
;; =======================

(require 'server)
(when (equal window-system 'w32)
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.
(server-start)

;; =======================
;; Key bindings
;; =======================
(require 'james-bindings)


;; =======================
;; Smex.  Must be at end of .emacs
;; =======================

(require 'smex)
(smex-initialize)
;;set up alternate alt key
(global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key (kbd "C-x m") 'smex)

(put 'downcase-region 'disabled nil)
(put 'autopair-newline 'disabled nil)
