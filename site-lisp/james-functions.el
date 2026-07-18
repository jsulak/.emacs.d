;;; james-functions.el --- Custom utility functions -*- lexical-binding: t; -*-

(require 'dired)

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


(defun james/isearch-occur ()
  "Run `occur' using the current isearch string."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp
               isearch-string
             (regexp-quote isearch-string)))))


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
  "Rename the current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless (and filename (file-exists-p filename))
      (user-error "Current buffer is not visiting an existing file"))
    (let ((new-name (read-file-name "New name: " filename)))
      (when-let ((other-buffer (find-buffer-visiting new-name)))
        (unless (eq other-buffer (current-buffer))
          (user-error "File is already visited by buffer %s"
                      (buffer-name other-buffer))))
      (unless (file-equal-p filename new-name)
        (rename-file filename new-name 1)
        (set-visited-file-name new-name nil t)
        (set-buffer-modified-p nil)))))


(defun james/move-buffer-file (dir)
  "Move the current buffer's visited file to DIR."
  (interactive "DNew directory: ")
  (let ((filename (buffer-file-name)))
    (unless (and filename (file-exists-p filename))
      (user-error "Current buffer is not visiting an existing file"))
    (let ((new-name (expand-file-name (file-name-nondirectory filename) dir)))
      (when-let ((other-buffer (find-buffer-visiting new-name)))
        (unless (eq other-buffer (current-buffer))
          (user-error "File is already visited by buffer %s"
                      (buffer-name other-buffer))))
      (unless (file-equal-p filename new-name)
        (rename-file filename new-name 1)
        (set-visited-file-name new-name nil t)
        (set-buffer-modified-p nil)))))



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



;; Reload init.el on the fly.
(defun james/reload-dot-emacs ()
  "Save and reload `user-init-file'."
  (interactive)
  (let ((init-file (or user-init-file
                       (expand-file-name "init.el" user-emacs-directory))))
    (when-let ((init-buffer (find-buffer-visiting init-file)))
      (with-current-buffer init-buffer
        (save-buffer)))
    (load-file init-file)
    (message "Emacs configuration reloaded successfully")))


;; XSteve functions
(defvar james/split-window-configuration nil
  "Window configuration before `james/split-window' was called.")

(defun james/split-window ()
  "Split the window and show the next buffer below.
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
        "Comment or uncomment the current line or region.
Replaces default `comment-dwim' end-of-line behavior."
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

(defun james/paste-markdown-as-org ()
  "Convert clipboard contents from Markdown to Org and insert at point.
Requires pandoc to be installed."
  (interactive)
  (let ((md-text (current-kill 0 t)))
    (unless md-text
      (user-error "Nothing on the kill ring"))
    (let ((org-text
           (with-temp-buffer
             (insert md-text)
             (when (zerop
                    (shell-command-on-region
                     (point-min) (point-max)
                     "pandoc -f markdown -t org --wrap=preserve"
                     (current-buffer) t))
               (goto-char (point-min))
               (while (re-search-forward
                       "^[ \t]*:PROPERTIES:\n\\(?:.*\n\\)*?[ \t]*:END:\n"
                       nil t)
                 (replace-match ""))
               ;; Ensure a blank line after headings
               (goto-char (point-min))
               (while (re-search-forward
                       "^\\(\\*+ .*\\)\n\\([^*\n]\\)"
                       nil t)
                 (replace-match "\\1\n\n\\2"))
               (buffer-string)))))
      (unless org-text
        (user-error "Pandoc conversion failed"))
      (insert org-text))))


(defun james/org-copy-rich-text ()
  "Copy current Org buffer or region as rich text (HTML) to the macOS clipboard."
  (interactive)
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (org-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert org-text)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f org -t html | textutil -stdin -format html -convert rtf -stdout | pbcopy"
       nil nil nil nil))
    (message "Copied as rich text.")))


(provide 'james-functions)

