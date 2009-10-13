;; append load path to load my customizations
(setq load-path
       (append load-path
 	      '("~/.emacs.d/")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Conditionally load os-specific initialization files
(if (or (eq system-type 'gnu/linux)
	(eq system-type 'linux))
    (load-file "~/.emacs.d/james-linux.el")
  (load-file "~/.emacs.d/james-windows.el"))

(load-file "~/.emacs.d/org-mode-settings.el")


;; Move scroll bar to right
(setq scroll-bar-mode-explicit t) 
(set-scroll-bar-mode `right) 

;; Get rid of the menu bar
(menu-bar-mode 0)

;; Delete files into trash
(setq delete-by-moving-to-trash t)

;; Have typing get rid of the active selection
(delete-selection-mode t)

;;Add js2 mode for javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;(global-set-key "\C-x\ r" 'recentf-open-files)

;;(require 'sr-speedbar)
;;(global-set-key [(super ?s)] 'sr-speedbar-toggle)

;; Use cperl-mode instead of the default perl-mode
;;(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;; (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

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
;;(setq cperl-electric-keywords t) ;; expands for keywords such as
                                 ;; foreach, while, etc...
;; (setq cperl-electric-parens t)

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


;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(setq inhibit-splash-screen t)


;;Add color themes
(setq load-path (append load-path '("~/.emacs.d/themes/")))
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/zenburn.el")
;;(color-theme-zenburn)
(load-file "~/.emacs.d/ruby-blue-theme.el")
(color-theme-ruby-blue)

;; Toggle line numbers
(require 'linum)
;;(global-linum-mode t)

;; ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
;(require 'template-simple)

;;Add nxml mode
(load "~/.emacs.d/nxml-mode/rng-auto.el")

(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|xslt\\|rng\\|xhtml\\|xpr\\|xspec\\|xpl\\)\\'" . nxml-mode)
	      auto-mode-alist))

;;auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;;paren highlighting
(show-paren-mode 1)

;;electric pairs
;;(pair-mode 1)

;;Add pymacs

;;(autoload 'pymacs-apply "pymacs")
;;(autoload 'pymacs-call "pymacs")
;;(autoload 'pymacs-eval "pymacs" nil t)
;;(autoload 'pymacs-exec "pymacs" nil t)
;;(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;;add ropemacs

;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")


;;Add ECB stuff

;;(add-to-list 'load-path "~/.emacs.d/eieio-0.17")

;;(add-to-list 'load-path "~/.emacs.d/semantic-1.4.4")
;;(setq semantic-load-turn-everything-on t)
;;(require 'semantic-load)


;;(add-to-list 'load-path "~/.emacs.d/speedbar-0.14beta4")
;; (autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;; (autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)

;;(add-to-list 'load-path
;;                   "~/.emacs.d/ecb-2.32")
;;(require 'ecb)


;;The following is from http://wttools.sourceforge.net/emacs-stuff/emacs.html

;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)

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


;;Add warnings before killing modified buffers
;;(defun maybe-kill-buffer ()
;;  (if (and (not buffer-file-name)
;;           (buffer-modified-p)
;;	   (not (equal (buffer-name) "*Open Recent*")))
      ;; buffer is not visiting a file
;;      (y-or-n-p "This buffer is not visiting a file but has been edited.  Kill it anyway? ")
 ;;   t))

;;(add-to-list 'kill-buffer-query-functions 'maybe-kill-buffer)




;;Add smart tab
;;(load-file "~/.emacs.d/smart-tab.el")


;;Add ruby support
(require 'ruby-mode)
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (define-key ruby-mode-map "C-m" 'newline-and-indent) ;Not sure if this line is 100% right but it works!
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))



;;(defun try-complete-abbrev (old)
;;(if (expand-abbrev) t nil))
;;(setq hippie-expand-try-functions-list
;;Set ctrl-z to undo
(global-set-key "\C-z" 'undo)

;;'(try-complete-abbrev
;;try-complete-file-name
;;try-expand-dabbrev))

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


;;Add Tab bar
;(require 'tabbar)
;(tabbar-mode 1)
;;(setq tabbar-buffer-groups-function
;;          (lambda (buffer)
;;            (list "All buffers")))
;; (dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
;;      (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
;;     
;;    (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
;;      `(defun ,name (arg)
;;         (interactive "P")
;;         ,do-always
;;         (if (equal nil arg)
;;             ,on-no-prefix
;;           ,on-prefix)))
;;     
;;    (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
;;    (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
;;     
;;    (global-set-key [(control tab)] 'shk-tabbar-next)
;;    (global-set-key [(control shift tab)] 'shk-tabbar-prev)
;;
;;(when (require 'tabbar nil t)
;;      (setq tabbar-buffer-groups-function
;;    	(lambda (b) (list "All Buffers")))
;;      (setq tabbar-buffer-list-function
;;    	(lambda ()
;;    	  (remove-if
;;    	   (lambda(buffer)
;;    	     (find (aref (buffer-name buffer) 0) " *"))
;;    	   (buffer-list))))
;;      (tabbar-mode 1))


;; Keyboard macros

;; Formats entire buffer
(fset 'indent-all
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 104 134217848 105 110 100 101 110 116 45 114 101 103 105 111 110 return 21 67108896 21 67108896] 0 "%d")) arg)))

;; Finds all the TODO and NOTE items in source code
(fset 'find-todo
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217843 111 84 79 68 79 92 124 78 79 84 69 58 left left left left left left left 58 end return] 0 "%d")) arg)))


;;http://www.xsteve.at/prg/emacs/power-user-tips.html
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))



(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)


(require 'bubble-buffer)
(setq bubble-buffer-omit-regexp "\\(^ .+$\\|\\*Messages\\*\\|*compilation\\*\\|\\*.+output\\*$\\|\\*TeX Help\\*$\\|\\*vc-diff\\*\\|\\*Occur\\*\\|\\*grep\\*\\|\\*cvs-diff\\*\\)")



;; To use resize-minibuffer-mode, uncomment this and include in your .emacs:
;;(resize-minibuffer-mode)


;; window frame title
(setq frame-title-format "%b (%f) - emacs")
(setq icon-title-format "emacs [%b]")



;;Add ACL mode mode for javascript
(autoload 'acl-mode "acl-mode")
(setq auto-mode-alist (append (list (cons "\\.acl\\'" 'acl-mode))
                               auto-mode-alist))



;;
;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
;;
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)
;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Turn on emacs server
;;(server-start)

;;Prevent backup files from being made
(setq make-backup-files nil)

;;Import external functions file
(require 'efuncs)
(require 'etags)

(require 'python)
(require 'yasnippet)

;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;;Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)


(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;(require 'auto-complete)
;(global-auto-complete-mode t)
;(require 'auto-complete-yasnippet)
;(set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-files-in-current-dir ac-source-symbols))

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



;; Add nav menu
(require 'nav)

;;Add grep Buffers
(require 'grep-buffers)

;; Activate winner mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; Add line highlighting
(global-hl-line-mode 1)
;;(set-face-background 'hl-line "#1A2B3D")
(set-face-background 'hl-line "#19293A")


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
(global-set-key [(f8)] 'swap-windows)
(global-set-key [(f9)] 'nav)

;;Set keybinding for functions in efunc.el
(global-set-key [(meta f10)] 'my-ido-find-tag)

(global-set-key [f11] 'bubble-buffer-next)
(global-set-key [(shift f11)] 'bubble-buffer-previous)
(global-set-key [(meta f11)] 'xsteve-ido-choose-from-recentf)

(global-set-key [(meta f12)] 'recentf-open-files)
(global-set-key [(f12)] 'ibuffer)

(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window

(global-set-key (kbd "\C-x 4") 'xsteve-split-window)

(global-set-key [(C-return)] 'dabbrev-expand)

;;Set auto-revert interval to be faster
(setq auto-revert-interval 2)


;; Install smex.  Must be at end of .emacs
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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


(put 'dired-find-alternate-file 'disabled nil)
