;; append load path to load my customizations
(setq load-path
       (append load-path
 	      '("~/.emacs.d/")))
(setq load-path
      (append load-path
	      '("~/.emacs.d/external")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Conditionally load os-specific initialization files
(if (or (eq system-type 'gnu/linux)
	(eq system-type 'linux))
    (load-file "~/.emacs.d/james-linux.el")
  (load-file "~/.emacs.d/james-windows.el"))

(load-file "~/.emacs.d/org-mode-settings.el")

;; Load custom function
(require 'efuncs)


;; ================================
;; Appearance 
;; ================================

;; Move scroll bar to right
(setq scroll-bar-mode-explicit t) 
(set-scroll-bar-mode `right) 
(setq inhibit-splash-screen t)
(menu-bar-mode 0)

;; window frame title
(setq frame-title-format "%b (%f) - emacs")
(setq icon-title-format "emacs [%b]")

;;Add color themes
(setq load-path (append load-path '("~/.emacs.d/themes/")))
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/external/ruby-blue-theme.el")
(color-theme-ruby-blue)

(require 'linum)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

;; Add line highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#19293A")

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; ================================
;; Behavior
;; ================================

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


;; ========================
;; Major modes 
;; ========================

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
(load "~/.emacs.d/nxml-mode/rng-auto.el")
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


;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)



;; =========================
;; External packages
;; =========================


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


;; =======================
;; Keybindings
;; =======================

;;Set ctrl-z to undo
(global-set-key "\C-z" 'undo)

;;set up alternate alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;set up kill word keyboard bindings
(global-set-key "\C-w" 'kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-q" 'backward-kill-word)

;; Run shell command
(global-set-key (kbd "\C-c s") 'shell-command)

;; Set goto-line
(global-set-key "\C-x\C-g" 'goto-line)

;;bind repeat last macro to F5
(global-set-key [(f4)] 'call-last-kbd-macro)
(global-set-key [(f5)] 'revert-buffer)

;;Provide short-cut for swap-windows (F8)
(global-set-key [(f6)] 'swap-windows)
;(global-set-key [(f9)] 'nav)

;;Set keybinding for functions in efunc.el
(global-set-key [(meta f10)] 'my-ido-find-tag)

;(global-set-key [f11] 'bubble-buffer-next)
;(global-set-key [(shift f11)] 'bubble-buffer-previous)
(global-set-key [(meta f11)] 'xsteve-ido-choose-from-recentf)

(global-set-key [(meta f12)] 'recentf-open-files)
(global-set-key [(f7)] 'ibuffer)

(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window

(global-set-key (kbd "\C-x 4") 'xsteve-split-window)

(global-set-key [(C-return)] 'dabbrev-expand)


;; Org-mode key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "<f10>") 'org-remember)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> i") (lambda ()
                                 (interactive)
                                 (info "~/git/org-mode/doc/org.info")))
(global-set-key (kbd "<f9> o") 'org-occur)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f12>") 'org-agenda)
;;Set auto-revert interval to be faster
;;(setq auto-revert-interval 2)

(global-set-key "\M-g" 'goto-line)




;; Install smex.  Must be at end of .emacs
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
