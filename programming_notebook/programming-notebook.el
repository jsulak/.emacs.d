
(defvar pn-mode-keymap (make-keymap) "Programming notebook keymap.")
(define-key pn-mode-keymap (kbd "C-<f10>") 'pn-insert-chrome-link)
(define-key pn-mode-keymap (kbd "C-c f") 'pn-force-line-breaks)

(defgroup programming-notebook nil
  "Programming notebook"
  :group 'text
  :prefix "pn-")

(defcustom pn-notebook-dir "~/Dropbox/programming_notebook/"
  "Directory containing programming notebook"
  :type 'string
  :group 'programming-notebook)

(defcustom pn-date-format "%Y%m%d"
  "Date format of daily entries in programming notebook"
  :type 'string
  :group 'programming-notebook)

(define-minor-mode programming-notebook-mode
  "Toggle Programming notebook mode."  
  :init-value nil
  ;; Mode line indicator
  :lighter " Prog-Notebook"
  :keymap pn-mode-keymap
  :group 'programming-notebook)


(defun pn-open-todays-entry ()
  "Open today's programming notebook entry."
  (interactive)
  (find-file
   (format
    (concat (file-name-as-directory pn-notebook-dir) "%s.md")
    (format-time-string pn-date-format)))
  (programming-notebook-mode)
  )

(defun pn-insert-chrome-link ()
  "Insert the title and url of active Chrome tab, formatted as Markdown."
  (interactive)
  (setq link-cmd (concat (file-name-as-directory pn-notebook-dir) "chrome_link.scpt"))
  (insert (shell-command-to-string (format "osascript %s" link-cmd))))

(defun pn-force-line-breaks ()
  "Inserts two spaces at the end of the lines within a region to force Markdown to insert line breaks."
  (interactive
   (if (region-active-p) 
       (replace-regexp "$" "  "))))


(provide 'programming-notebook)