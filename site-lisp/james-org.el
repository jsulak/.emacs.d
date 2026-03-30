;;; james-org.el --- Org mode configuration -*- lexical-binding: t; -*-

(require 'org)

;; Set org directories in local.el
;;(setq org-directory "~/org")
;;(setq org-agenda-files '("~/org"))

(setq org-agenda-span 10)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "ON HOLD(h)" "|" "DONE(d)" "OBE(c)")))
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(600))
(add-hook 'org-mode-hook 'auto-save-visited-mode)
(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2)))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

(require 'rich2org)

;; (use-package org-modern
;;   :hook ((org-mode . org-modern-mode)
;;          (org-agenda-finalize . org-modern-agenda)))

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d%H%M%S-"
        org-download-screenshot-method "screencapture -i %s"
        org-download-annotate-function (lambda (_link) ""))
  ;; Enable drag-and-drop on macOS
  (setq dnd-protocol-alist
        '(("^file:" . org-download-dnd)
          ("^http" . org-download-dnd)))

  ;; Open image files in Preview.app when clicked or via C-c C-o
  (with-eval-after-load 'org
    (dolist (ext '("\\.png\\'" "\\.jpg\\'" "\\.jpeg\\'" "\\.gif\\'" "\\.webp\\'"))
      (add-to-list 'org-file-apps (cons ext "open -a Preview.app %s")))
    (define-key org-mode-map [double-mouse-1]
      (lambda (event)
        (interactive "e")
        (mouse-set-point event)
        (when (get-char-property (point) 'org-image-overlay)
          (org-open-at-point)))))
  :hook (org-mode . org-download-enable))


(defun james/org-return ()
  "In a checkbox list item, RET creates a new checkbox item.
On an empty checkbox item, remove it and insert a newline.
Otherwise, normal return."
  (interactive)
  (if (and (org-in-item-p)
           (save-excursion
             (beginning-of-line)
             (looking-at "\\s-*- \\[.\\]")))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*- \\[.\\]\\s-*$"))
          ;; Empty checkbox item — remove it and exit
          (progn
            (delete-region (line-beginning-position) (line-end-position))
            (delete-char -1)  ; remove the trailing newline
            (org-return))
        ;; Non-empty checkbox item — create a new one
        (org-insert-item 'checkbox))
    (org-return)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") #'james/org-return))


(defun james/org-sort-checkboxes ()
  "Sort checkbox list, unchecked first."
  (interactive)
  (org-sort-list nil ?f
    (lambda ()
      (if (looking-at ".*\\[X\\]") 1 0))
    #'<))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
    '("w" "Waiting/Owed to me"
      todo "WAITING"
      ((org-agenda-sorting-strategy '(deadline-up scheduled-up))))))

;; Auto-convert markdown links to org links on paste
(defun james/markdown-to-org-link-on-yank (orig-fun &rest args)
  "After yanking in org-mode, convert [title](url) to [[url][title]]."
  (apply orig-fun args)
  (when (derived-mode-p 'org-mode)
    (let ((end (point))
          (beg (mark t)))
      (when (and beg end)
        (save-excursion
          (goto-char (min beg end))
          (while (re-search-forward "\\[\\([^]]+\\)](\\([^)]+\\))" (max beg end) t)
            (let* ((title (match-string 1))
                   (url (match-string 2)))
              (replace-match (format "[[%s][%s]]" url title) t t))))))))

(advice-add 'yank :around #'james/markdown-to-org-link-on-yank)

;; =======================
;; Capture templates
;; =======================

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("l" "Open Loop" entry
                 (file+headline "open-loops.org" "Open Loops")
                 "* WAITING %^{Who} - %^{What}\nSCHEDULED: %^t\n"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("L" "Open Loop (deadline)" entry
                 (file+headline "open-loops.org" "Open Loops")
                 "* WAITING %^{Who} - %^{What}\nDEADLINE: %^t\n"
                 :empty-lines 1)))

;; Keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(provide 'james-org)
;;; james-org.el ends here
