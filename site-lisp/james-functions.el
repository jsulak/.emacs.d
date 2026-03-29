;;; james-functions.el --- Custom utility functions -*- lexical-binding: t; -*-

(defun james/slick-copy-advice (orig-fun &rest args)
  "When called interactively with no active region, copy a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'interactive)))
      (apply orig-fun args)
    (message "Copied line")
    (funcall orig-fun (line-beginning-position) (line-beginning-position 2))))
(advice-add 'kill-ring-save :around #'james/slick-copy-advice)

(defun james/slick-cut-advice (orig-fun &rest args)
  "When called interactively with no active region, kill a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'interactive)))
      (apply orig-fun args)
    (funcall orig-fun (line-beginning-position) (line-beginning-position 2))))
(advice-add 'kill-region :around #'james/slick-cut-advice)


;; Adds extra keybinding to interactive search that sends the current term to occur
;; From http://emacsblog.org/page/5/
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
	(interactive)
	(let ((case-fold-search isearch-case-fold-search))
	  (occur (if isearch-regexp isearch-string
			   (regexp-quote isearch-string))))))


;;These are from Steve Yegge's blog post

(defun james/swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (car (window-list)))
	 (w2 (cadr (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))


(defun james/rename-file-and-buffer ()
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


(defun james/move-buffer-file (dir)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Increase/Decrease font size on the fly
;;; Taken from: http://is.gd/iaAo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun james/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun james/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))



;Reload .emacs on the fly
(defun james/reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer "init.el"))
      (save-buffer(get-buffer "init.el")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))


;; XSteve functions
(defun james/split-window ()
  "Split the current window and show in the window below the next buffer in the buffer list.
When called twice restore the window configuration before the split."
  (interactive)
  (if (eq last-command 'james/split-window)
      (progn
        (set-window-configuration james/split-window-configuration)
        (setq this-command 'james/unsplit-window))
    (let ((buf-list)
          (cur-buf (current-buffer)))
      (setq james/split-window-configuration (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (setq buf-list (buffer-list))
      (delq (get-buffer " *Minibuf-0*") buf-list)
      (delq (get-buffer " *Minibuf-1*") buf-list)
      (pop-to-buffer (cadr buf-list))
      (pop-to-buffer cur-buf)
      (other-window '1))))

;; Filter files in dired down to a regex
;; From http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/acb20ee78c00e4ec#
(defun james/dired-show-only (regexp)
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))


;; Duplicate and (optionally) comment out a line:
(defun james/duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original" 
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

(defun james/indent-all ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


;; Frim http://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun james/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; from http://www.emacswiki.org/emacs/CommentingCode
 (defun james/comment-dwim-line (&optional arg)
        "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
          (interactive "*P")
          (comment-normalize-vars)
          (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))
            (comment-dwim arg)))



(defun james/delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^(<[\"'") (setq p1 (point))
      (skip-chars-forward "^)>]\"'") (setq p2 (point))
      (delete-region p1 p2))))


;; Pretty-prints a json string
;; if region is selected, then uses that,
;; else does entire buffer.
(defun james/pretty-print-json ()
  (interactive)
  (let ((b (if (region-active-p) (region-beginning) (point-min)))
        (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e "python3 -mjson.tool" (current-buffer) t)))

;; http://whattheemacsd.com//editing-defuns.el-01.html
(defun james/open-line-below ()
  (interactive)
  (end-of-line)
  (newline) 
  (indent-for-tab-command))

(defun james/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun james/font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
   This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'james/font-lock-comment-annotations)

(defun james/revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(provide 'james-functions)

