;;; james-markdown.el --- Markdown and Zettelkasten configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown presentation and Zettelkasten navigation helpers.

;;; Code:

(require 'markdown-mode)
(require 'elec-pair)

;;; Editing

(defun james/markdown-disable-quote-pairing ()
  "Prevent automatic double-quote pairing in Markdown buffers."
  (setq-local electric-pair-pairs
              (assq-delete-all ?\" (copy-tree electric-pair-pairs)))
  (setq-local electric-pair-text-pairs
              (assq-delete-all ?\" (copy-tree electric-pair-text-pairs)))
  (add-function :before-until (local 'electric-pair-inhibit-predicate)
                (lambda (char) (eq char ?\"))))

(add-hook 'markdown-mode-hook #'james/markdown-disable-quote-pairing)

;;; Appearance

(setq markdown-enable-wiki-links t)
(setq markdown-wiki-link-fontify-missing t)
(setq markdown-wiki-link-search-type '(parent-directories))
(setq markdown-hide-markup t)
(setq markdown-fontify-code-blocks-natively t)
(setq markdown-list-item-bullets '("-"))

(set-face-attribute 'markdown-header-face-1 nil :height 1.3 :weight 'bold)
(set-face-attribute 'markdown-header-face-2 nil :height 1.2 :weight 'bold)
(set-face-attribute 'markdown-header-face-3 nil :height 1.1 :weight 'semi-bold)
(set-face-attribute 'markdown-header-face-4 nil :height 1.05 :weight 'semi-bold)

(add-hook 'markdown-mode-hook #'iimage-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-fill-column-mode)
(add-hook 'markdown-mode-hook (lambda () (setq line-spacing 0.3)))

;;; Indent display mode

(defface james/markdown-heading-hide
  '((t :inherit default))
  "Face for hidden leading heading markers in markdown.
Foreground is set to match the background after theme load."
  :group 'markdown-faces)

(defun james/markdown-indent--update-heading-hide-face ()
  "Match the hidden Markdown heading marker face to the background."
  (set-face-foreground 'james/markdown-heading-hide (face-background 'default)))

(james/markdown-indent--update-heading-hide-face)
(add-hook 'after-load-theme-functions
          (lambda (_) (james/markdown-indent--update-heading-hide-face)))

(defun james/markdown-indent--fontify-headings (limit)
  "Fix heading display properties through LIMIT during font locking.
Removes display=\"\" from heading markers, hides leading #s, keeps last # visible."
  (when (re-search-forward "^\\(#\\{1,6\\}\\) " limit t)
    (let* ((hashes-start (match-beginning 1))
           (hashes-end (match-end 1))
           (space-pos hashes-end)
           (level (- hashes-end hashes-start)))
      (remove-text-properties hashes-start (1+ space-pos) '(display nil))
      (when (> level 1)
        (put-text-property hashes-start (+ hashes-start (1- level))
                           'face 'james/markdown-heading-hide))
      (put-text-property (+ hashes-start (1- level)) hashes-end
                         'face 'markdown-header-delimiter-face))
    t))

(defun james/markdown-indent--jit-lock (start end)
  "Update `line-prefix' and `wrap-prefix' in the region START to END."
  (save-excursion
    (goto-char start)
    (setq start (line-beginning-position))
    (let ((current-indent ""))
      (save-excursion
        (when (re-search-backward "^\\(#\\{1,6\\}\\) " nil t)
          (setq current-indent (make-string (* (length (match-string 1)) 2) ?\s))))
      (goto-char start)
      (while (and (not (eobp)) (<= (point) end))
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (cond
           ((looking-at "^\\(#\\{1,6\\}\\) ")
            (setq current-indent (make-string (* (length (match-string 1)) 2) ?\s))
            (put-text-property line-start line-end 'line-prefix "")
            (put-text-property line-start line-end 'wrap-prefix ""))
           ((looking-at "^[ \t]*$"))
           ((looking-at "^\\([ \t]*\\)\\([*+-]\\|[0-9]+\\.\\) ")
            (let* ((marker-end (- (match-end 0) line-start))
                   (hang-prefix (concat current-indent (make-string marker-end ?\s))))
              (put-text-property line-start line-end 'line-prefix current-indent)
              (put-text-property line-start line-end 'wrap-prefix hang-prefix)))
           (t
            (put-text-property line-start line-end 'line-prefix current-indent)
            (put-text-property line-start line-end 'wrap-prefix current-indent))))
        (forward-line 1)))))

(define-minor-mode james/markdown-indent-mode
  "Visually indent markdown body text under headings."
  :lighter " MdInd"
  (if james/markdown-indent-mode
      (progn
        (font-lock-add-keywords nil '((james/markdown-indent--fontify-headings)) 'append)
        (jit-lock-register #'james/markdown-indent--jit-lock)
        (font-lock-flush))
    (font-lock-remove-keywords nil '((james/markdown-indent--fontify-headings)))
    (jit-lock-unregister #'james/markdown-indent--jit-lock)
    (remove-text-properties (point-min) (point-max) '(line-prefix nil wrap-prefix nil))
    (font-lock-flush)))

(add-hook 'markdown-mode-hook #'james/markdown-indent-mode)

;;; Zettelkasten

(defvar james/zk-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Zettel"
  "Directory containing zettelkasten markdown notes.")

(define-minor-mode james/zk-mode
  "Follow numeric wiki links to notes in `james/zk-directory'."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [double-mouse-1] #'james/zk-follow-link-on-double-click)
            map))

(defun james/zk--maybe-enable-mode ()
  "Enable `james/zk-mode' for Markdown files in the Zettelkasten."
  (when (and buffer-file-name
             (stringp james/zk-directory)
             (file-in-directory-p buffer-file-name
                                  (expand-file-name james/zk-directory)))
    (james/zk-mode 1)))

(add-hook 'markdown-mode-hook #'james/zk--maybe-enable-mode)

(defun james/zk--file-for-uid (uid)
  "Return the Markdown note in `james/zk-directory' matching UID."
  (let ((matches (directory-files
                  (expand-file-name james/zk-directory) t
                  (format "\\`%s\\(?: .*\\)?\\.md\\'" (regexp-quote uid)))))
    (pcase (length matches)
      (0 (user-error "No Zettel note found for ID %s" uid))
      (1 (car matches))
      (_ (user-error "Multiple Zettel notes found for ID %s" uid)))))

(defun james/zk--follow-wiki-link (original name &optional other)
  "Follow numeric wiki link NAME, or call ORIGINAL for other links.
Open in another window when OTHER is non-nil."
  (if (and james/zk-mode (string-match-p "\\`[0-9]+\\'" name))
      (let ((file (james/zk--file-for-uid name)))
        (if other (find-file-other-window file) (find-file file)))
    (funcall original name other)))

(advice-add 'markdown-follow-wiki-link :around #'james/zk--follow-wiki-link)

(defun james/zk-follow-link-on-double-click (event)
  "Follow a numeric wiki link at double-click EVENT, or just move point."
  (interactive "e")
  (mouse-set-point event)
  (when (and (markdown-wiki-link-p)
             (string-match-p "\\`[0-9]+\\'" (markdown-wiki-link-link)))
    (markdown-follow-wiki-link-at-point)))

(defun james/zk--extract-uid (filename)
  "Extract the leading timestamp UID from FILENAME."
  (when (string-match "\\`\\([0-9]+\\)" (file-name-nondirectory filename))
    (match-string 1 (file-name-nondirectory filename))))

(defun james/zk-backlinks ()
  "Find notes linking to the current file's UID."
  (interactive)
  (let ((uid (james/zk--extract-uid (buffer-file-name))))
    (unless uid (user-error "No UID found in filename"))
    (consult-ripgrep james/zk-directory (concat "\\[\\[" uid "\\]\\]"))))

(defun james/zk-insert-link ()
  "Insert a [[wikilink]] by selecting from existing notes."
  (interactive)
  (let* ((files (directory-files james/zk-directory nil "\\.md$"))
         (choice (completing-read "Link to: " files))
         (uid (james/zk--extract-uid choice)))
    (unless uid (user-error "No UID found in filename"))
    (insert (format "[[%s]]" uid))))

(provide 'james-markdown)
;;; james-markdown.el ends here
