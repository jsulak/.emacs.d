;; rich2org.el — Paste clipboard rich text as Org mode content
;;
;; Drop this in your config (e.g. after (use-package org ...))
;; Requires: pandoc, macOS (uses osascript)
;;
;; Usage:
;;   M-x my/paste-rich-as-org    — inserts converted Org at point
;;   M-x my/rich-to-org-buffer   — opens result in a new Org buffer

(defvar my/rich2org-script
  (expand-file-name "bin/rich2org.sh" user-emacs-directory)
  "Path to the rich2org.sh script.")

(defun my/--clipboard-html-to-org ()
  "Extract HTML from clipboard and convert to Org. Returns string or nil."
  (let ((org (shell-command-to-string
              (shell-quote-argument my/rich2org-script))))
    (if (string-empty-p (string-trim org))
        (progn (message "rich2org: No output (no rich text on clipboard?).") nil)
      org)))

;;;###autoload
(defun my/paste-rich-as-org ()
  "Convert clipboard rich text to Org and insert at point.
Useful when copying from Outlook, OneNote, or any rich-text source."
  (interactive)
  (let ((org (my/--clipboard-html-to-org)))
    (when org
      (insert org)
      (message "rich2org: Inserted %d chars of Org content."
               (length org)))))

;;;###autoload
(defun my/rich-to-org-buffer ()
  "Convert clipboard rich text to Org and open in a new buffer."
  (interactive)
  (let ((org (my/--clipboard-html-to-org)))
    (when org
      (let ((buf (generate-new-buffer "*rich2org*")))
        (with-current-buffer buf
          (org-mode)
          (insert org)
          (goto-char (point-min)))
        (pop-to-buffer buf)
        (message "rich2org: Done. Review and yank what you need.")))))

;; Optional: bind to keys
;; (global-set-key (kbd "C-c v") #'my/paste-rich-as-org)
;; (global-set-key (kbd "C-c V") #'my/rich-to-org-buffer)

(provide 'rich2org)
