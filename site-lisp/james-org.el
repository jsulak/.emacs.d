;;; james-org.el --- Org mode configuration -*- lexical-binding: t; -*-

(require 'org)

;; Defaults: Set org directories in local.el
(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))

(setq org-agenda-span 10)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "ON HOLD(h)" "|" "DONE(d)" "OBE(c)")))
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(500))
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(add-hook 'org-mode-hook 'auto-save-visited-mode)
(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.3)))

(set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.1 :weight 'semi-bold)
(set-face-attribute 'org-level-4 nil :height 1.05 :weight 'semi-bold)

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 90)
  (visual-fill-column-center-text t))

(require 'rich2org)

(setq org-file-apps
      '((directory . "open %s")
        (auto-mode . emacs)))

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


;; (defun james/org-return ()
;;   "In a list item, RET creates a new list item of the same type.
;; Checkbox items get a new checkbox, ordered/unordered items get a plain item.
;; On an empty list item, remove it and insert a newline.
;; Otherwise, normal return."
;;   (interactive)
;;   (let ((on-list-line (save-excursion
;;                         (beginning-of-line)
;;                         (looking-at "\\s-*\\([-+*]\\|[0-9]+[.)]\\)\\( \\[.\\]\\)?\\s-"))))
;;     (if (and (org-in-item-p) on-list-line)
;;         (let ((checkbox-p (save-excursion
;;                             (beginning-of-line)
;;                             (looking-at "\\s-*[-+*] \\[.\\]\\|\\s-*[0-9]+[.)] \\[.\\]")))
;;               (empty-p (save-excursion
;;                          (beginning-of-line)
;;                          (looking-at "\\s-*\\([-+*]\\|[0-9]+[.)]\\)\\( \\[.\\]\\)?\\s-*$"))))
;;           (if empty-p
;;               ;; Empty list item — remove it and exit
;;               (progn
;;                 (delete-region (line-beginning-position) (line-end-position))
;;                 (delete-char -1)
;;                 (org-return))
;;             ;; Non-empty list item — create a new one
;;             (org-insert-item checkbox-p)))
;;       (org-return))))

;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "RET") #'james/org-return))



(defun james/org-focus-heading ()
  "Collapse everything, then reveal current subtree."
  (interactive)
  (org-overview)
  (org-reveal t)
  (org-show-subtree))

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
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               `("n" "New file" plain
                 (file ,(lambda ()
                          (let ((name (read-string "Name: ")))
                            (setq james/--capture-file-title name)
                            (expand-file-name (concat name ".org") org-directory))))
                 "#+TITLE: %(identity james/--capture-file-title)\n#+DATE: %t\n#+FILETAGS: %(let ((tags (read-string \"Tags (comma-separated): \"))) (concat \":\" (mapconcat #'string-trim (split-string tags \",\") \":\") \":\"))\n\n%?"
                 :immediate-finish nil))

  (add-to-list 'org-capture-templates
               '("o" "1:1 Agenda Item" checkitem
                 (file+function james/org-1on1-file james/org-1on1-find-person)
                 "- [ ] %?")))

(defun james/org-insert-file-link ()
  "Insert an org link to a file in `org-directory' using minibuffer completion.
Uses the file's #+TITLE as the link description, falling back to the filename."
  (interactive)
  (let* ((files (directory-files org-directory nil "\\.org\\'"))
         (choice (completing-read "Link to org file: " files nil t))
         (path (expand-file-name choice org-directory))
         (title (with-temp-buffer
                  (insert-file-contents path nil 0 1024)
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\+TITLE:[ \t]+\\(.+\\)" nil t)
                      (match-string 1)
                    (file-name-sans-extension choice)))))
    (insert (format "[[file:%s][%s]]" choice title))))

;; =======================
;; 1:1 Agenda System
;; =======================

(defvar james/org-1on1-file (expand-file-name "agendas.org" org-directory)
  "File for 1:1 agenda items.")

(defvar james/org-agenda-person nil
  "Current person search term for agenda TODO filtering.")

(defvar james/org-agenda-person-full nil
  "Full heading name resolved from `james/org-agenda-person'.")

(defun james/org-1on1-people ()
  "Return list of people from agendas.org headings."
  (org-map-entries
   (lambda () (nth 4 (org-heading-components)))
   "LEVEL=1" (list james/org-1on1-file)))

(defun james/org-1on1-find-person ()
  "Jump to a person's heading in agendas.org, creating if needed."
  (let* ((people (james/org-1on1-people))
         (person (completing-read "Person: " people nil nil)))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s" (regexp-quote person)) nil t)
        (org-end-of-subtree t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" person)))))

(defun james/org-agenda-skip-unless-person ()
  "Skip entry unless its heading or body mentions the current person.
Matches any word from the person's name (first or last)."
  (let ((end (save-excursion (org-end-of-subtree t)))
        (case-fold-search t)
        (words (split-string james/org-agenda-person)))
    (save-excursion
      (if (cl-some (lambda (word)
                     (save-excursion
                       (re-search-forward (regexp-quote word) end t)))
                   words)
          nil
        end))))

(defun james/org-1on1-agenda-block ()
  "Custom agenda block showing unchecked items for `james/org-agenda-person'."
  (let ((inhibit-read-only t)
        items)
    (with-current-buffer (find-file-noselect james/org-1on1-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward
              (format "^\\* %s" (regexp-quote james/org-agenda-person-full)) nil t)
         (let ((end (save-excursion (org-end-of-subtree t))))
           (while (re-search-forward "^\\s-*- \\[ \\] \\(.*\\)" end t)
             (push (cons (match-string 1)
                         (copy-marker (match-beginning 0)))
                   items))))))
    (setq items (nreverse items))
    (insert (propertize "1:1 Agenda Items\n" 'face 'org-agenda-structure))
    (if items
        (dolist (item items)
          (let ((line (concat "  [ ] " (car item) "\n")))
            (add-text-properties
             0 (length line)
             (list 'org-marker (cdr item)
                   'org-hd-marker (cdr item)
                   'mouse-face 'highlight
                   'help-echo "RET/TAB to jump to item")
             line)
            (insert line)))
      (insert "  No pending items\n"))
    (insert "\n")))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
    '("p" "Person View"
      ((funcall (lambda () (james/org-1on1-agenda-block)))
       (todo "TODO\\|WAITING"
             ((org-agenda-overriding-header "Tasks")
              (org-agenda-sorting-strategy
               '(deadline-up scheduled-up priority-down))
              (org-agenda-skip-function
               '(james/org-agenda-skip-unless-person))))))))

(defun james/org-1on1-resolve-person (input people)
  "Resolve INPUT to a full name from PEOPLE list.
Matches exact, then prefix (first name), then substring."
  (or (car (member input people))
      (cl-find-if (lambda (p) (string-prefix-p input p t)) people)
      (cl-find-if (lambda (p) (string-match-p (regexp-quote input) p)) people)
      input))

(defun james/org-agenda-person-view ()
  "Show agenda filtered to a specific person."
  (interactive)
  (let* ((people (james/org-1on1-people))
         (input (completing-read "Person: " people nil nil))
         (full (james/org-1on1-resolve-person input people)))
    (setq james/org-agenda-person input
          james/org-agenda-person-full full)
    (org-agenda nil "p")))

(defun james/org-1on1-add-item (person item)
  "Add ITEM under PERSON in agendas.org non-interactively."
  (with-current-buffer (find-file-noselect james/org-1on1-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (if (re-search-forward (format "^\\* %s" (regexp-quote person)) nil t)
         (progn
           (org-end-of-subtree t)
           (unless (bolp) (insert "\n")))
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert (format "* %s\n" person)))
     (insert (format "- [ ] %s\n" item))
     (save-buffer))
    (format "Added to %s: %s" person item)))

;; Keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c f") 'james/org-insert-file-link)
(global-set-key (kbd "C-c p") #'james/org-agenda-person-view)



(provide 'james-org)
;;; james-org.el ends here
