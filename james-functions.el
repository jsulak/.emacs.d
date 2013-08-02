;;This document contains miscellaneous functions, for now all from

;;These are from Steve Yegge's blog post

(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))



;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 


;; This is from a stack overflow answer
;; It creates an ido-replacement function for finding tags

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Increase/Decrease font size on the fly
;;; Taken from: http://is.gd/iaAo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ryan/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun ryan/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'ryan/increase-font-size)
(global-set-key (kbd "C--") 'ryan/decrease-font-size)



;Reload .emacs on the fly
(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer "init.el"))
      (save-buffer(get-buffer "init.el")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))


;; XSteve functions
(defun xsteve-split-window ()
  "Split the current window and show in the window below the next buffer in the buffer list.
When called twice restore the window configuration before the split."
  (interactive)
  (if (eq last-command 'xsteve-split-window)
      (progn
        (set-window-configuration xsteve-split-window-configuration)
        (setq this-command 'xsteve-unsplit-window))
    (let ((buf-list)
          (cur-buf (current-buffer)))
      (setq xsteve-split-window-configuration (current-window-configuration))
      (delete-other-windows)
      (split-window-horizontally)
      (setq buf-list (buffer-list))
      (delq (get-buffer " *Minibuf-0*") buf-list)
      (delq (get-buffer " *Minibuf-1*") buf-list)
      (pop-to-buffer (cadr buf-list))
      (pop-to-buffer cur-buf)
      (other-window '1))))

;;; Hack dired to launch files with 'l' key.  Put this in your ~/.emacs file
;;; From http://omniorthogonal.blogspot.com/2008/05/useful-emacs-dired-launch-hack.html
(defun dired-launch-command ()
  (interactive)
  (dired-do-shell-command 
   (case system-type       
     (gnu/linux "gnome-open") ;right for gnome (ubuntu), not for other systems
     (darwin "open")
     (windows-nt "open"))
   nil
   (dired-get-marked-files t current-prefix-arg)))

;; Filter files in dired down to a regex
;; From http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/acb20ee78c00e4ec#
(defun dired-show-only (regexp)
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))


;; Duplicate and (optionally) comment out a line:
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original" 
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; or choose some better bindings....


;; Open a windows explorer window at the location of the current buffer
;; from http://zhangda.wordpress.com/2010/02/03/open-the-path-of-the-current-buffer-within-emacs/
(defun open-buffer-path ()
"Run explorer on the directory of the current buffer."
(interactive)
(shell-command (concat "explorer " (replace-regexp-in-string "/" "\\\\" (file-name-directory (buffer-file-name)) t t))))



;; Formats entire buffer
(fset 'indent-all
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 104 134217848 105 110 100 101 110 116 45 114 101 103 105 111 110 return 21 67108896 21 67108896] 0 "%d")) arg)))

;; Finds all the TODO and NOTE items in source code
(fset 'find-todo
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217843 111 84 79 68 79 92 124 78 79 84 69 58 left left left left left left left 58 end return] 0 "%d")) arg)))

;; From: http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(fset 'indent-all-xml
   [?\M-< ?\C-  ?\M-> ?\M-x ?p ?r ?e ?t ?t ?y return])


;; Frim http://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

;; from http://www.emacswiki.org/emacs/CommentingCode
 (defun comment-dwim-line (&optional arg)
        "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
          (interactive "*P")
          (comment-normalize-vars)
          (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))
            (comment-dwim arg)))




(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a 
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
   
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
   
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurence of CHAR.")

(defun delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^(<[\"'") (setq p1 (point))
      (skip-chars-forward "^)>]\"'") (setq p2 (point))
      (delete-region p1 p2))))

 ;; (defun ido-goto-symbol (&optional symbol-list)
 ;;      "Refresh imenu and jump to a place in the buffer using Ido."
 ;;      (interactive)
 ;;      (unless (featurep 'imenu)
 ;;        (require 'imenu nil t))
 ;;      (cond
 ;;       ((not symbol-list)
 ;;        (let ((ido-mode ido-mode)
 ;;              (ido-enable-flex-matching
 ;;               (if (boundp 'ido-enable-flex-matching)
 ;;                   ido-enable-flex-matching t))
 ;;              name-and-pos symbol-names position)
 ;;          (unless ido-mode
 ;;            (ido-mode 1)
 ;;            (setq ido-enable-flex-matching t))
 ;;          (while (progn
 ;;                   (imenu--cleanup)
 ;;                   (setq imenu--index-alist nil)
 ;;                   (ido-goto-symbol (imenu--make-index-alist))
 ;;                   (setq selected-symbol
 ;;                         (ido-completing-read "Symbol? " symbol-names))
 ;;                   (string= (car imenu--rescan-item) selected-symbol)))
 ;;          (unless (and (boundp 'mark-active) mark-active)
 ;;            (push-mark nil t nil))
 ;;          (setq position (cdr (assoc selected-symbol name-and-pos)))
 ;;          (cond
 ;;           ((overlayp position)
 ;;            (goto-char (overlay-start position)))
 ;;           (t
 ;;            (goto-char position)))))
 ;;       ((listp symbol-list)
 ;;        (dolist (symbol symbol-list)
 ;;          (let (name position)
 ;;            (cond
 ;;             ((and (listp symbol) (imenu--subalist-p symbol))
 ;;              (ido-goto-symbol symbol))
 ;;             ((listp symbol)
 ;;              (setq name (car symbol))
 ;;              (setq position (cdr symbol)))
 ;;             ((stringp symbol)
 ;;              (setq name symbol)
 ;;              (setq position
 ;;                    (get-text-property 1 'org-imenu-marker symbol))))
 ;;            (unless (or (null position) (null name)
 ;;                        (string= (car imenu--rescan-item) name))
 ;;              (add-to-list 'symbol-names name)
 ;;              (add-to-list 'name-and-pos (cons name position))))))))


;; Pretty-prints a json string
;; if region is selected, then uses that,
;; else does entire buffer.
(defun pretty-print-json ()
  (interactive)
  (setq b
        (if (region-active-p)
            (region-beginning)
          (point-min)))
  (setq e
        (if (region-active-p)
            (region-end)
          (point-max)))
  (shell-command-on-region
   b
   e
   "python -mjson.tool"
   (current-buffer)
   t))

(defun toggle-indent-style ()
  (interactive)
  (if indent-tabs-mode
      (setq indent-tabs-mode nil)
    (setq indent-tabs-mode 1))
  )

  
;; http://whattheemacsd.com//editing-defuns.el-01.html
(defun open-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'james-functions)

