;;; james-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org capture, attachment, agenda, and personal workflow configuration.

;;; Code:

(require 'cl-lib)
(require 'dnd)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defvar org-agenda-custom-commands)
(defvar org-capture-templates)
(defvar james/--capture-file-title nil
  "Title entered while creating a new Org file through capture.")

(defconst james/org-open-loops-filename "todo-tasks.org"
  "Org file containing the central open-loops checklist.")

(defconst james/org-open-loops-heading "Open Loops"
  "Heading containing the central open-loops checklist.")

(defgroup james-org nil
  "Personal Org mode configuration."
  :group 'org)

(defcustom james/org-attachment-root "attachments"
  "Root directory used to store Org attachments.
Relative paths are resolved against `org-directory'.  Absolute paths may
place attachments outside the Org collection.  The Org file's collection-
relative path is mirrored below this directory without its extension."
  :type 'directory
  :group 'james-org)

;; Defaults: Set org directories in local.el
(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))

(setq org-agenda-span 10)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "ON HOLD(h)" "|" "DONE(d)" "OBE(c)")))
(setq org-tags-column 0)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(500))
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-use-sub-superscripts nil)
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
        ("\\.org\\(?:_archive\\)?\\'" . emacs)
        (t . "open %s")))

(defvar james/org-download-source-name nil
  "Source name override used while formatting an Org image filename.")

(defconst james/org-image-extensions
  '("avif" "bmp" "gif" "heic" "jpeg" "jpg" "png" "svg" "tif" "tiff"
    "webp")
  "Filename extensions routed through the Org image workflow.")

(defconst james/org-powerpoint-extensions
  '("pot" "potm" "potx" "pps" "ppsm" "ppsx" "ppt" "pptm" "pptx")
  "Filename extensions recognized as PowerPoint documents.")

(defconst james/org-word-extensions
  '("doc" "docm" "docx" "dot" "dotm" "dotx")
  "Filename extensions recognized as Word documents.")

(defconst james/org-excel-extensions
  '("xla" "xlam" "xls" "xlsb" "xlsm" "xlsx" "xlt" "xltm" "xltx")
  "Filename extensions recognized as Excel documents.")

(defconst james/org-other-document-extensions
  '("csv" "epub" "key" "markdown" "md" "numbers" "odp" "ods" "odt"
    "pages" "rtf" "tex" "txt")
  "Other document extensions that receive a generic link icon.")

(defun james/org-storage-directory (kind &optional org-file storage-root)
  "Return the KIND storage directory for ORG-FILE.
ORG-FILE defaults to the variable `buffer-file-name'.  Its path relative
to the Org collection is mirrored below STORAGE-ROOT.  When STORAGE-ROOT
is nil, use the collection's KIND directory.  Relative storage roots are
resolved against `org-directory'."
  (let* ((org-file (or org-file buffer-file-name))
         (root (file-name-as-directory (expand-file-name org-directory))))
    (unless org-file
      (user-error "Save this Org buffer before adding files"))
    (let* ((relative (file-relative-name (expand-file-name org-file) root))
           (first-component (car (split-string relative "[/\\\\]" t))))
      (when (equal first-component "..")
        (user-error "Org file is outside org-directory: %s" org-file))
      (expand-file-name
       (file-name-sans-extension relative)
       (file-name-as-directory
        (expand-file-name (or storage-root kind) root))))))

(defun james/org-image-directory (&optional org-file)
  "Return the image directory for ORG-FILE under `org-directory'."
  (james/org-storage-directory "images" org-file))

(defun james/org-attachment-directory (&optional org-file)
  "Return the configured attachment directory for ORG-FILE."
  (james/org-storage-directory
   "attachments" org-file james/org-attachment-root))

(defun james/org-image--sanitized-name (filename)
  "Return a lowercase, filesystem-friendly image name for FILENAME."
  (let* ((filename (file-name-nondirectory filename))
         (extension (file-name-extension filename))
         (stem (downcase (file-name-base filename))))
    (setq stem (replace-regexp-in-string "[^[:alnum:]]+" "-" stem)
          stem (string-trim stem "-+" "-+"))
    (when extension
      (setq extension
            (replace-regexp-in-string "[^[:alnum:]]" "" (downcase extension))))
    (concat (if (string-empty-p stem) "image" stem)
            (if (and extension (not (string-empty-p extension)))
                (concat "." extension)
              ""))))

(defun james/org-image--unique-name (directory filename)
  "Return an available FILENAME in DIRECTORY, adding a numeric suffix."
  (if (not (file-exists-p (expand-file-name filename directory)))
      filename
    (let* ((extension (file-name-extension filename t))
           (stem (file-name-sans-extension filename))
           (suffix 2)
           candidate)
      (while
          (progn
            (setq candidate (format "%s-%d%s" stem suffix (or extension ""))
                  suffix (1+ suffix))
            (file-exists-p (expand-file-name candidate directory))))
      candidate)))

(defun james/org--timestamp-prefix ()
  "Return the compact timestamp prefix used for Org file names."
  (format-time-string "%Y%m%d%H%M-"))

(defun james/org-download-file-name (filename)
  "Format org-download FILENAME using the Org image naming convention."
  (let* ((source (or james/org-download-source-name filename))
         (sanitized (james/org-image--sanitized-name source))
         (timestamped (concat (james/org--timestamp-prefix) sanitized)))
    (james/org-image--unique-name (james/org-image-directory) timestamped)))

(defun james/org-attachment-file-name (filename)
  "Format FILENAME using the Org attachment naming convention."
  (let* ((sanitized (james/org-image--sanitized-name filename))
         (timestamped (concat (james/org--timestamp-prefix) sanitized)))
    (james/org-image--unique-name
     (james/org-attachment-directory) timestamped)))

(defun james/org-image-file-p (filename)
  "Return non-nil when FILENAME has a supported image extension."
  (member (downcase (or (file-name-extension filename) ""))
          james/org-image-extensions))

(defun james/org-attachment-file-p (filename)
  "Return non-nil when FILENAME should use the attachment workflow."
  (let ((extension (downcase (or (file-name-extension filename) ""))))
    (and (not (string-empty-p extension))
         (not (james/org-image-file-p filename))
         (not (member extension '("org" "org_archive"))))))

(defun james/org--link-target-extension (target)
  "Return the lowercase filename extension from link TARGET."
  (let* ((filename (url-filename (url-generic-parse-url target)))
         (filename (car (split-string (or filename "") "[?#]"))))
    (downcase (or (file-name-extension (url-unhex-string filename)) ""))))

(defun james/org--document-link-icon (target &optional attachment)
  "Return an icon for document TARGET, or nil.
When ATTACHMENT is non-nil, return the generic attachment icon for any
target that is not a recognized PowerPoint, Word, Excel, or PDF document."
  (let ((extension (james/org--link-target-extension target)))
    (cond
     ((member extension james/org-powerpoint-extensions) "📊")
     ((member extension james/org-word-extensions) "📄")
     ((member extension james/org-excel-extensions) "📈")
     ((equal extension "pdf") "📕")
     ((or attachment
          (member extension james/org-other-document-extensions))
      "📎"))))

(defun james/org--link-description (target description &optional attachment)
  "Add TARGET's document icon to DESCRIPTION when appropriate.
ATTACHMENT has the same meaning as in `james/org--document-link-icon'."
  (save-match-data
    (if-let ((icon (or (james/org--document-link-icon target attachment)
                       (james/org--document-link-icon description))))
        (format "%s %s" icon description)
      description)))

(defun james/org-attachment--local-file (uri)
  "Return the local filename represented by URI, or nil."
  (let* ((parsed (url-generic-parse-url uri))
         (host (url-host parsed)))
    (when (and (equal (url-type parsed) "file")
               (member host (list nil "" "localhost" (system-name))))
      (url-unhex-string (url-filename parsed)))))

(defun james/org-attachment-insert (source)
  "Copy SOURCE into this Org file's attachment directory and insert a link."
  (interactive "fAttach file: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Attachments can only be added from an Org buffer"))
  (let* ((source (expand-file-name source))
         (directory (james/org-attachment-directory))
         (source-extension (downcase (or (file-name-extension source) ""))))
    (unless (file-regular-p source)
      (user-error "Attachment is not a regular file: %s" source))
    (when (or (james/org-image-file-p source)
              (member source-extension '("org" "org_archive")))
      (user-error "Use the image or Org link workflow for: %s" source))
    (make-directory directory t)
    (let ((target
           (if (file-in-directory-p source directory)
               source
             (expand-file-name
              (james/org-attachment-file-name source) directory))))
      (unless (file-equal-p source target)
        (copy-file source target nil t nil t))
      (insert (org-link-make-string
               (concat "file:"
                       (org-link-escape (file-relative-name target)))
               (james/org--link-description
                source (file-name-nondirectory source) t))
              "\n")
      target)))

(defun james/org-attachment-download (uri)
  "Download the attachment at URI and insert a relative Org link."
  (let* ((parsed (url-generic-parse-url uri))
         (source-name
          (file-name-nondirectory (url-unhex-string (url-filename parsed))))
         (directory (james/org-attachment-directory)))
    (unless (james/org-attachment-file-p source-name)
      (user-error "URI does not identify an attachment: %s" uri))
    (make-directory directory t)
    (let ((target
           (expand-file-name
            (james/org-attachment-file-name source-name) directory)))
      (url-copy-file uri target nil)
      (insert (org-link-make-string
               (concat "file:"
                       (org-link-escape (file-relative-name target)))
               (james/org--link-description uri source-name t))
              "\n")
      target)))

(defun james/org-download-dnd-with-attachments (orig-fun uri action)
  "Route non-image URI drops to attachments, otherwise call ORIG-FUN."
  (let* ((parsed (url-generic-parse-url uri))
         (source (james/org-attachment--local-file uri))
         (remote-name
          (file-name-nondirectory (url-unhex-string (url-filename parsed)))))
    (cond
     ((and (derived-mode-p 'org-mode)
           source
           (file-regular-p source)
           (james/org-attachment-file-p source))
      (james/org-attachment-insert source)
      action)
     ((and (derived-mode-p 'org-mode)
           (member (url-type parsed) '("ftp" "http" "https"))
           (james/org-attachment-file-p remote-name))
      (james/org-attachment-download uri)
      action)
     (t
      (funcall orig-fun uri action)))))

(defun james/org-download-directory (orig-fun)
  "Use the collection image directory in Org, otherwise call ORIG-FUN."
  (if (derived-mode-p 'org-mode)
      (let ((directory (james/org-image-directory)))
        (make-directory directory t)
        directory)
    (funcall orig-fun)))

(defun james/org-download-clipboard-without-id-property (orig-fun
                                                         &optional basename)
  "Call ORIG-FUN without an ID drawer, naming the image from BASENAME."
  (let ((basename (or basename "clipboard.png")))
    (cl-letf (((symbol-function 'org-id-get-create) #'ignore))
      (funcall orig-fun basename))))

(defun james/org-download-base64-with-image-name (orig-fun &rest args)
  "Call ORIG-FUN with ARGS using image.png as its source filename."
  (let ((james/org-download-source-name "image.png"))
    (apply orig-fun args)))

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method 'directory
        org-download-file-format-function #'james/org-download-file-name
        org-download-screenshot-method "screencapture -i %s"
        org-download-annotate-function (lambda (_link) ""))
  ;; Resolve the complete directory here so org-download's buffer-local
  ;; heading setting cannot append another path component.
  (advice-add 'org-download--dir :around #'james/org-download-directory)
  (advice-add 'org-download-clipboard :around
              #'james/org-download-clipboard-without-id-property)
  (advice-add 'org-download-dnd-base64 :around
              #'james/org-download-base64-with-image-name)
  (advice-add 'org-download-dnd :around
              #'james/org-download-dnd-with-attachments)
  ;; Open image files in Preview.app when clicked or via C-c C-o
  (with-eval-after-load 'org
    (dolist (ext '("\\.png\\'" "\\.jpg\\'" "\\.jpeg\\'" "\\.gif\\'" "\\.webp\\'"))
      (add-to-list 'org-file-apps (cons ext "open -a Preview.app %s"))))
  :hook (org-mode . org-download-enable))

(use-package ox-clip
  :ensure t)


(defun james/org-focus-heading ()
  "Collapse everything, then reveal current subtree."
  (interactive)
  (org-overview)
  (org-reveal t)
  (org-fold-show-subtree))

(defun james/org-open-inline-image (event)
  "Open the inline Org image clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when (get-char-property (point) 'org-image-overlay)
    (org-open-at-point)))

(defun james/org-emphasize-bold ()
  "Emphasize the active Org region using bold markup."
  (interactive)
  (org-emphasize ?*))

(defun james/org-emphasize-italic ()
  "Emphasize the active Org region using italic markup."
  (interactive)
  (org-emphasize ?/))

(defun james/org-emphasize-underline ()
  "Emphasize the active Org region using underline markup."
  (interactive)
  (org-emphasize ?_))

(defun james/org-sort-checkboxes ()
  "Sort checkbox list, unchecked first."
  (interactive)
  (org-sort-list nil ?f
    (lambda ()
      (if (looking-at ".*\\[X\\]") 1 0))
    #'<))

(defun james/org-sort-checkboxes-by-date ()
  "Sort current Org list by checkbox state, then timestamp.
Unchecked items come first.  Within each checkbox state, dated items are
sorted chronologically and undated items are placed last."
  (interactive)
  (org-sort-list
   nil ?f
   (lambda ()
     (save-excursion
       (let ((checked (looking-at-p ".*\\[X\\]"))
             (end (line-end-position)))
         (format "%d:%s"
                 (if checked 1 0)
                 (if (re-search-forward org-ts-regexp-both end t)
                     (match-string 0)
                   "<9999-12-31>")))))
   #'string<))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
    '("w" "Waiting/Owed to me"
      todo "WAITING"
      ((org-agenda-sorting-strategy '(deadline-up scheduled-up))))))

;; Auto-convert markdown links to org links on paste
(defun james/markdown-to-org-link-on-yank (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, then convert Markdown links in `org-mode'."
  (apply orig-fun args)
  (when (derived-mode-p 'org-mode)
    (let ((end (point))
          (beg (mark t)))
      (when (and beg end)
        (save-restriction
          (narrow-to-region (min beg end) (max beg end))
          (goto-char (point-min))
          (while (re-search-forward "\\[\\([^]]+\\)](\\([^)]+\\))" nil t)
            (let ((title (match-string 1))
                  (url (match-string 2)))
              (replace-match
               (format "[[%s][%s]]"
                       url (james/org--link-description url title))
               t t))))))))

(advice-add 'yank :around #'james/markdown-to-org-link-on-yank)

;; =======================
;; Capture templates
;; =======================

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               `("l" "Open Loop" plain
                 (file+function james/org-open-loops-file
                                james/org-open-loops-goto-heading)
                 "- [ ] %^{Who} - %^{What}\n"
                 :empty-lines-after 1))

  (add-to-list 'org-capture-templates
               `("L" "Open Loop (dated)" plain
                 (file+function james/org-open-loops-file
                                james/org-open-loops-goto-heading)
                 "- [ ] %^{Who} - %^{What} %^t\n"
                 :empty-lines-after 1))

  (add-to-list 'org-capture-templates
               `("n" "New file" plain
                 (file ,(lambda ()
                          (let ((name (read-string "Name: ")))
                            (setq james/--capture-file-title name)
                            (expand-file-name (concat name ".org") org-directory))))
                 "#+TITLE: %(identity james/--capture-file-title)\n#+DATE: %t\n#+FILETAGS: %(let ((tags (read-string \"Tags (comma-separated): \"))) (concat \":\" (mapconcat #'string-trim (split-string tags \",\") \":\") \":\"))\n\n%?"
                 :immediate-finish nil))

  (add-to-list 'org-capture-templates
               '("o" "1:1 Agenda Item" plain
                 (file+function james/org-1on1-choose-person-file james/org-1on1-goto-topics)
                 "- [ ] %?\n")))

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


(defun james/org-archive-done-tasks ()
  "Archive all tasks with a DONE or OBE state in the current buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE|OBE" 'file))


;; =======================
;; 1:1 Agenda System
;; =======================

(defvar james/org-agenda-person nil
  "Current person search term for agenda TODO filtering.")

(defvar james/org-agenda-person-full nil
  "Full heading name resolved from `james/org-agenda-person'.")

(defun james/org-open-loops-file ()
  "Return the path to the central open-loops file."
  (expand-file-name james/org-open-loops-filename org-directory))

(defun james/org-open-loops--find-heading ()
  "Return a marker for the open-loops heading, ignoring title case."
  (save-excursion
    (goto-char (point-min))
    (let ((target (downcase james/org-open-loops-heading))
          marker)
      (while (and (not marker) (re-search-forward org-heading-regexp nil t))
        (when (equal (downcase (org-get-heading t t t t)) target)
          (setq marker (point-marker))))
      marker)))

(defun james/org-open-loops-goto-heading ()
  "Move point to the capture position below the open-loops heading."
  (if-let ((marker (james/org-open-loops--find-heading)))
      (progn
        (goto-char marker)
        (org-end-of-subtree t))
    (user-error "Open-loops heading not found in %s"
                (james/org-open-loops-file))))

(defun james/org-agenda--mentions-person-p (text)
  "Return non-nil when TEXT mentions the current agenda person.
Matches any word from `james/org-agenda-person', ignoring case."
  (let ((case-fold-search t))
    (cl-some (lambda (word)
               (string-match-p (regexp-quote word) text))
             (split-string james/org-agenda-person))))

(defun james/org-1on1-people ()
  "Return list of people from org files tagged with :person:."
  (let (people)
    (dolist (file (directory-files org-directory t "\\.org\\'"))
      (with-temp-buffer
        (insert-file-contents file nil 0 512)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+FILETAGS:.*:person:" nil t)
          (goto-char (point-min))
          (push (if (re-search-forward "^#\\+TITLE:[ \t]+\\(.+\\)" nil t)
                    (string-trim (match-string 1))
                  (file-name-sans-extension (file-name-nondirectory file)))
                people))))
    (nreverse people)))

(defun james/org-1on1-person-file (person)
  "Return path to PERSON's org file, creating it if needed."
  (let ((file (expand-file-name (concat person ".org") org-directory)))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (format "#+TITLE: %s\n#+FILETAGS: :person:\n\n* Topics\n\n" person))))
    file))

(defvar james/org-1on1--capture-file nil
  "Temporary storage for the person file path during capture.")

(defun james/org-1on1-choose-person-file ()
  "Prompt for a person and return their org file path."
  (let* ((people (james/org-1on1-people))
         (person (completing-read "Person: " people nil nil)))
    (setq james/org-1on1--capture-file (james/org-1on1-person-file person))
    james/org-1on1--capture-file))

(defun james/org-1on1-goto-topics ()
  "Position point for new item under Topics heading, creating if needed.
Ensures a blank line after the heading, then positions at end of list."
  (goto-char (point-min))
  (unless (re-search-forward "^\\* Topics" nil t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* Topics"))
  (end-of-line)
  (let ((has-items (re-search-forward "^- \\[" (save-excursion (org-end-of-subtree t) (point)) t)))
    (if has-items
        (progn
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n")))
      (insert "\n\n"))))

(defun james/org-agenda-skip-unless-person ()
  "Skip entry unless its heading or body mentions the current person.
Matches any word from the person's name (first or last)."
  (james/org-1on1-ensure-person)
  (let ((end (save-excursion (org-end-of-subtree t))))
    (if (or (and buffer-file-name
                 (equal (expand-file-name buffer-file-name)
                        (james/org-open-loops-file))
                 (equal (downcase (org-get-heading t t t t))
                        (downcase james/org-open-loops-heading)))
            (not (james/org-agenda--mentions-person-p
                  (buffer-substring-no-properties (point) end))))
        end
      nil)))

(defun james/org-open-loops--items ()
  "Return unchecked open loops matching the current agenda person.
Each result is a cons cell containing the item text and its source marker."
  (james/org-1on1-ensure-person)
  (let ((file (james/org-open-loops-file))
        items)
    (when (file-readable-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when-let ((heading (james/org-open-loops--find-heading)))
           (goto-char heading)
           (let ((end (save-excursion (org-end-of-subtree t))))
             (while (re-search-forward "^\\s-*- \\[ \\] \\(.*\\)" end t)
               (let ((text (match-string-no-properties 1)))
                 (when (james/org-agenda--mentions-person-p text)
                   (push (cons text (copy-marker (match-beginning 0)))
                         items)))))))))
    (nreverse items)))

(defun james/org-agenda--insert-checkbox-block (heading items empty-message)
  "Insert an agenda block named HEADING for checkbox ITEMS.
EMPTY-MESSAGE is displayed when ITEMS is nil.  Each item must be a cons cell
containing display text and a source marker."
  (let ((inhibit-read-only t))
    (insert (propertize (concat heading "\n") 'face 'org-agenda-structure))
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
      (insert (concat "  " empty-message "\n")))
    (insert "\n")))

(defun james/org-1on1-agenda-block ()
  "Custom agenda block showing unchecked items for `james/org-agenda-person'."
  (james/org-1on1-ensure-person)
  (let ((file (james/org-1on1-person-file james/org-agenda-person-full))
        items)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward "^\\* Topics" nil t)
         (let ((end (save-excursion (org-end-of-subtree t))))
           (while (re-search-forward "^\\s-*- \\[ \\] \\(.*\\)" end t)
             (push (cons (match-string 1)
                         (copy-marker (match-beginning 0)))
                   items))))))
    (james/org-agenda--insert-checkbox-block
     "1:1 Agenda Items" (nreverse items) "No pending items")))

(defun james/org-open-loops-agenda-block ()
  "Insert pending open loops for `james/org-agenda-person' in the agenda."
  (james/org-agenda--insert-checkbox-block
   "Open Loops" (james/org-open-loops--items) "No pending open loops"))

(defun james/org-agenda-person-select ()
  "Prompt for the person used by the custom person agenda."
  (setq james/org-agenda-person nil
        james/org-agenda-person-full nil)
  (james/org-1on1-ensure-person))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
    '("p" "Person View"
      ((funcall (lambda () (james/org-agenda-person-select)))
       (funcall (lambda () (james/org-1on1-agenda-block)))
       (funcall (lambda () (james/org-open-loops-agenda-block)))
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

(defun james/org-1on1-ensure-person ()
  "Ensure `james/org-agenda-person' variables are set, prompting if needed."
  (unless james/org-agenda-person-full
    (let* ((people (james/org-1on1-people))
           (input (completing-read "Person: " people nil nil))
           (full (james/org-1on1-resolve-person input people)))
      (setq james/org-agenda-person input
            james/org-agenda-person-full full))))

(defun james/org-agenda-person-view ()
  "Show agenda filtered to a specific person."
  (interactive)
  (org-agenda nil "p"))

(defun james/org-1on1-add-item (person item)
  "Add ITEM under PERSON's Topics heading non-interactively.
Creates the person's file if it doesn't exist."
  (let ((file (james/org-1on1-person-file person)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward "^\\* Topics" nil t)
           (progn
             (org-end-of-subtree t)
             (unless (bolp) (insert "\n"))
             (when (save-excursion (forward-line -1) (looking-at-p "^\\* "))
               (insert "\n")))
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert "* Topics\n\n"))
       (insert (format "- [ ] %s\n" item))
       (save-buffer))
      (format "Added to %s: %s" person item))))

(provide 'james-org)
;;; james-org.el ends here
