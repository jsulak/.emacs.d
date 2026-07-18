;;; james-markdown.el --- Markdown and Zettelkasten configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown presentation and Zettelkasten navigation helpers.

;;; Code:

(require 'markdown-mode)

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
Foreground is set to match the background after theme load.")

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
