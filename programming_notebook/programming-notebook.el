
(defvar pn-notebook-dir "~/Dropbox/programming_notebook/")

(defun pn-open-todays-entry ()
  (interactive)
  (find-file
   (format
    (concat (file-name-as-directory pn-notebook-dir) "%s.md")
    (format-time-string "%Y%m%d"))))

(defun pn-insert-chrome-link ()
  (interactive)
  (setq link-cmd (concat (file-name-as-directory pn-notebook-dir) "chrome_link.scpt"))
  (insert (shell-command-to-string (format "osascript %s" link-cmd))))

(provide 'programming-notebook)
