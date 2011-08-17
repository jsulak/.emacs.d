;; append load path to load my customizations
(setq load-path
       (append load-path
 	      '("~/.emacs.d/")))
(setq load-path
      (append load-path
	      '("~/.emacs.d/external")))

(setq load-path
      (append load-path
	      '("~/.emacs.d/external/emacs-color-theme-solarized")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(cond ((or (eq system-type 'gnu/linux)
	  (eq system-type 'linux))
       (load-file "~/.emacs.d/james-linux.el"))
      ((eq system-type 'windows-nt)
       (load-file "~/.emacs.d/james-windows.el")))
             
(load-file "~/.emacs.d/org-mode-settings.el")


;; Load custom functions
(require 'efuncs)


;; ================================
;; Appearance 
;; ================================

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode 0)

(require 'linum)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)


;; ================================
;; Behavior
;; ================================

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set initial mode to text-mode
(setq-default initial-major-mode 'text-mode)

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

(ido-mode 1)
(setq ido-enable-flex-matching t)

;;auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

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
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; M-SPC not available, window manager take it away
(global-set-key (kbd "M-'") 'just-one-space)
(global-set-key (kbd "C-M-=") 'pde-indent-dwim)
;; nearest key to dabbrev-expand
(global-set-key (kbd "M-;") 'hippie-expand)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c f") 'comint-dynamic-complete)

(setq hippie-expand-try-functions-list
          '(try-expand-line
            try-expand-dabbrev
            try-expand-line-all-buffers
            try-expand-list
            try-expand-list-all-buffers
            try-expand-dabbrev-visible
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name
            try-complete-file-name-partially
            try-complete-lisp-symbol
            try-complete-lisp-symbol-partially
            try-expand-whole-kill))
(autoload 'comint-dynamic-complete "comint" "Complete for file name" t)
(setq comint-completion-addsuffix '("/" . ""))
;;(setq-default indent-tabs-mode nil)

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

;;(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
;;(add-to-list 'auto-mode-alist '("\\log\\'" . log4j-mode))

(setq python-check-command "pyflakes")

(defalias 'perl-mode 'cperl-mode)
(defun pde-perl-mode-hook ()
  (abbrev-mode t)
  (add-to-list 'cperl-style-alist
               '("PDE"
                 (cperl-auto-newline                         . t)
                 (cperl-brace-offset                         . 0)
                 (cperl-close-paren-offset                   . -4)
                 (cperl-continued-brace-offset               . 0)
                 (cperl-continued-statement-offset           . 4)
                ;; (cperl-extra-newline-before-brace           . nil)
                ;; (cperl-extra-newline-before-brace-multiline . nil)
                 (cperl-indent-level                         . 4)
                 (cperl-indent-parens-as-block               . t)
                 (cperl-label-offset                         . -4)
                 (cperl-merge-trailing-else                  . t)
                 (cperl-tab-always-indent                    . t)))
  (cperl-set-style "PDE"))

(setq cperl-invalid-face nil) 

;;Add nxml mode
;;(load "~/.emacs.d/nxml-mode/rng-auto.el")
;;(push "b:/scripts/catalog/emacs-catalog.xml" rng-schema-locating-files-default)
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|xslt\\|rng\\|xhtml\\|xpr\\|xspec\\|xpl\\)\\'" . nxml-mode)
	      auto-mode-alist))

;;Add js2 mode for javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;Add support for dos batch files
(require 'dosbat)
(setq auto-mode-alist
        (cons '("\\.\\(bat\\|cmd\\)\\'" . bat-mode)
	      auto-mode-alist))

;;Add support for xquery-mode
(require 'xquery-mode)
(setq auto-mode-alist
      (cons '("\\.\\(xqy\\|xquery\\|xq\\|xqm\\)\\'" . xquery-mode)
	    auto-mode-alist))

;;Add ACL mode mode 
(autoload 'acl-mode "acl-mode")
(setq auto-mode-alist (append (list (cons "\\.acl\\'" 'acl-mode))
                               auto-mode-alist))

(require 'csharp-mode)

(setq auto-mode-alist
(append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Add markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))



;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)





;; =========================
;; External packages
;; =========================

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 

;;The following is from http://wttools.sourceforge.net/emacs-stuff/emacs.html
;;; Excellent package for better scrolling in emacs
;;; should be default package. But now it can be downloaded
;;; from: http://user.it.uu.se/~mic/pager.el
(require 'pager)
     (global-set-key "\C-v"	   'pager-page-down)
     (global-set-key [next] 	   'pager-page-down)
     (global-set-key "\ev"	   'pager-page-up)
     (global-set-key [prior]	   'pager-page-up)
     (global-set-key '[M-up]    'pager-row-up)
     (global-set-key '[M-kp-8]  'pager-row-up)
     (global-set-key '[M-down]  'pager-row-down)
     (global-set-key '[M-kp-2]  'pager-row-down)


(require 'etags)
(require 'cl)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

(add-to-list 'load-path "~/.emacs.d/external/nav")
(require 'nav)
(require 'grep-buffers)
(require 'w32-browser)


;; Visible bookmakrs
(setq bm-restore-repository-on-load t)
(require 'bm)
(global-set-key (kbd "<M-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
 
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

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/external/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/external/autocomplete//ac-dict")

;; This turns off filename completion everywhere because it crashes in js.
;; It would be better to do it for js2-mode only
(defun ac-common-setup ())  
(ac-config-default)

(ac-set-trigger-key "TAB")
;;(setq ac-auto-start nil)
;;(setq ac-auto-show-menu nil)


;; ======================
;; Cursor control
;; ======================


;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
;;http://emacs-fu.blogspot.com/2009/12/changing-cursor-color-and-shape.html
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type

(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "yellow")
(setq djcb-normal-cursor-type    'box)

(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color djcb-read-only-color)
      (setq cursor-type djcb-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color djcb-overwrite-color)
      (setq cursor-type djcb-overwrite-cursor-type))
    (t 
      (set-cursor-color djcb-normal-color)
      (setq cursor-type djcb-normal-cursor-type))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)



;; =======================
;; Keybindings
;; =======================


;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)
;; duplicate a line and comment the first
(global-set-key (kbd "C-c c") (lambda()(interactive)(djcb-duplicate-line t)))
;; Join lines
(global-set-key "\C-x\C-j" 'join-line)

;;Set ctrl-z to undo
(global-set-key "\C-z" 'undo)


;;set up kill word keyboard bindings
(global-set-key "\C-w" 'kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-q" 'backward-kill-word)

;; Use ibuffer instead of normal buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Run shell command
(global-set-key (kbd "\C-c s") 'shell-command)

;; Set goto-line
(global-set-key "\C-x\C-g" 'goto-line)

;;bind repeat last macro to F5
(global-set-key [(f4)] 'call-last-kbd-macro)
(global-set-key [(f5)] 'revert-buffer)

;;Provide short-cut for swap-windows (F8)
(global-set-key [(f6)] 'swap-windows)
(global-set-key [(f7)] 'nav)

;; Point stack bindings
(global-set-key '[(f8)] 'point-stack-push)
(global-set-key '[(f9)] 'point-stack-pop)
(global-set-key '[(f10)] 'point-stack-forward-stack-pop)


;;Set keybinding for functions in efunc.el
;;(global-set-key [(meta f10)] 'my-ido-find-tag)

;(global-set-key [f11] 'bubble-buffer-next)
;(global-set-key [(shift f11)] 'bubble-buffer-previous)
(global-set-key [(meta f11)] 'xsteve-ido-choose-from-recentf)

(global-set-key [(meta f12)] 'recentf-open-files)
;(global-set-key [(f7)] 'ibuffer)

(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window

(global-set-key (kbd "\C-x 5") 'xsteve-split-window)

(global-set-key [(C-return)] 'dabbrev-expand)




;; Org-mode key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;(global-set-key (kbd "<f11>") 'org-clock-goto)
;(global-set-key (kbd "C-<f11>") 'org-clock-in)
;(global-set-key (kbd "<f10>") 'org-remember)
;(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;(global-set-key (kbd "<f9> b") 'bbdb)
;(global-set-key (kbd "<f9> c") 'calendar)
;(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;(global-set-key (kbd "<f9> i") (lambda ()
;                                 (interactive)
;                                 (info "~/git/org-mode/doc/org.info")))
;(global-set-key (kbd "<f9> o") 'org-occur)
;(global-set-key (kbd "<f9> r") 'boxquote-region)
;(global-set-key (kbd "<f12>") 'org-agenda)
;;Set auto-revert interval to be faster
;;(setq auto-revert-interval 2)


;; Redefine comment-dwim to comment out whole line
(global-set-key (kbd "C-;") 'comment-dwim-line)

;; Misc keybindings

(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c o") 'occur)
;(global-set-key (kbd "C-c i") 'indent-all-xml)
(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(global-set-key [(meta f10)] 'ido-goto-symbol)

(global-set-key (kbd "C-c p") 'pager-page-up)
(global-set-key (kbd "C-c n") 'pager-page-down)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)


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
;; Smex.  Must be at end of .emacs
;; =======================

(require 'smex)
(smex-initialize)
;;set up alternate alt key
(global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(put 'downcase-region 'disabled nil)
