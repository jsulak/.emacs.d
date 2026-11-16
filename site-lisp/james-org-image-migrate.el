;;; james-org-image-migrate.el --- Batch Org image migration -*- lexical-binding: t; -*-

;;; Commentary:
;; Plan, apply, and verify reviewed OCR-based image renames without loading the
;; interactive Emacs configuration.  The TSV manifest is the review boundary.

;;; Code:

(require 'cl-lib)
(require 'james-org-image)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defconst james/org-image-migrate-manifest-version "1"
  "Current image migration manifest format version.")

(defvar james/org-image-migrate-ocr-program "tesseract"
  "Executable used to OCR images while planning a migration.")

(defvar james/org-image-migrate-ocr-arguments '("-l" "eng" "--psm" "11")
  "Arguments following the input image and stdout destination.")

(defvar james/org-image-migrate-require-backup t
  "When non-nil, applying requires a verified backup archive.")

(cl-defstruct (james/org-image-migrate-row
               (:constructor james/org-image-migrate--make-row))
  action org-file line old-path new-path image-hash org-hash ocr-source
  ocr-title warning cache)

(defun james/org-image-migrate--hash-file (file)
  "Return the SHA256 hash of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun james/org-image-migrate--clean-text (text)
  "Normalize OCR TEXT identically to the interactive image workflow."
  (setq text (replace-regexp-in-string
              (regexp-opt '("\r\n" "\r" "\f" "\u2028" "\u2029"))
              "\n" text))
  (string-join
   (delq nil
         (mapcar
          (lambda (line)
            (let ((clean (string-trim
                          (replace-regexp-in-string
                           "[ \t\u00a0\u202f]+" " " line))))
              (unless (string-empty-p clean) clean)))
          (split-string text "\n")))
   "\n"))

(defun james/org-image-migrate--timestamp (filename)
  "Return FILENAME's timestamp normalized to YYYYMMDDHHMM, or nil."
  (let ((basename (file-name-nondirectory filename)))
    (cond
     ((string-match "\\`\\([0-9]\\{12\\}\\)-" basename)
      (match-string 1 basename))
     ((string-match
       "\\`\\([0-9]\\{8\\}\\)-\\([0-9]\\{4\\}\\)[0-9]\\{2\\}-"
       basename)
      (concat (match-string 1 basename) (match-string 2 basename))))))

(defun james/org-image-migrate--relative (file root)
  "Return FILE relative to ROOT using forward slashes."
  (file-relative-name (expand-file-name file) (file-name-as-directory root)))

(defun james/org-image-migrate--under-root-p (file root)
  "Return non-nil when FILE is located beneath ROOT."
  (file-in-directory-p (expand-file-name file)
                       (file-name-as-directory (expand-file-name root))))

(defun james/org-image-migrate--image-files (root)
  "Return all supported image files below ROOT's images directory."
  (let ((directory (expand-file-name "images" root)))
    (if (file-directory-p directory)
        (cl-remove-if-not #'james/org-image-file-p
                          (directory-files-recursively directory "."))
      nil)))

(defun james/org-image-migrate--drawer-after (link-end)
  "Return the OCR drawer element immediately after LINK-END, or nil."
  (save-excursion
    (goto-char link-end)
    (forward-line 1)
    (when (looking-at "[ \t]*:OCR:[ \t]*$")
      (let ((element (org-element-at-point)))
        (when (and (eq (org-element-type element) 'drawer)
                   (equal (org-element-property :drawer-name element) "OCR"))
          element)))))

(defun james/org-image-migrate--drawer-text (drawer expected-hash)
  "Return OCR text from generated DRAWER when its hash is EXPECTED-HASH."
  (when drawer
    (let* ((raw (buffer-substring-no-properties
                 (org-element-property :begin drawer)
                 (org-element-property :end drawer)))
           (lines (split-string raw "\n"))
           (hash-line (nth 1 lines))
           (options-line (nth 2 lines)))
      (when (and hash-line options-line
                 (string-match
                  "^[ \t]*: sha256: \\([0-9a-f]+\\)[ \t]*$" hash-line)
                 (equal (match-string 1 hash-line) expected-hash)
                 (string-match-p "^[ \t]*: options: " options-line))
        (james/org-image-migrate--clean-text
         (mapconcat
           (lambda (line)
             (replace-regexp-in-string "^[ \t]*:\\(?: \\|$\\)" "" line))
          (butlast (nthcdr 3 lines)) "\n"))))))

(defun james/org-image-migrate--run-ocr (file)
  "Run the configured OCR engine for FILE and return cleaned output."
  (unless (executable-find james/org-image-migrate-ocr-program)
    (error "Cannot find OCR executable: %s"
           james/org-image-migrate-ocr-program))
  (let ((output (generate-new-buffer " *Org image migration OCR*"))
        (errors (make-temp-file "org-image-migration-ocr-errors-")))
    (unwind-protect
        (let ((status
               (apply #'call-process james/org-image-migrate-ocr-program
                      nil (list output errors) nil file "stdout"
                      james/org-image-migrate-ocr-arguments)))
          (unless (and (integerp status) (zerop status))
            (error "OCR failed for %s: %s" file
                   (with-temp-buffer
                     (insert-file-contents errors)
                     (string-trim (buffer-string)))))
          (with-current-buffer output
            (james/org-image-migrate--clean-text (buffer-string))))
      (kill-buffer output)
      (delete-file errors))))

(defun james/org-image-migrate--run-title-ocr (file)
  "Return layout metadata for FILE's likely slide title."
  (let ((output (generate-new-buffer " *Org image migration title OCR*"))
        (errors (make-temp-file "org-image-migration-title-errors-")))
    (unwind-protect
        (let ((status
               (apply #'call-process james/org-image-migrate-ocr-program
                      nil (list output errors) nil file "stdout"
                      (append james/org-image-migrate-ocr-arguments '("tsv")))))
          (unless (and (integerp status) (zerop status))
            (error "Title OCR failed for %s: %s" file
                   (with-temp-buffer
                     (insert-file-contents errors)
                     (string-trim (buffer-string)))))
          (with-current-buffer output
            (james/org-image-title-from-tsv (buffer-string))))
      (kill-buffer output)
      (delete-file errors))))

(defun james/org-image-migrate--title-warning (title)
  "Return a review warning for suspicious OCR TITLE, or nil."
  (let ((alphanumeric
         (and title (replace-regexp-in-string "[^[:alnum:]]" "" title))))
    (cond
     ((or (null title) (string-empty-p alphanumeric)) "no-alphanumeric-title")
     ((<= (length alphanumeric) 2) "short-title")
     ((string-match-p "\\`Detected [0-9]+ diacritics\\'" title)
      "ocr-diagnostic"))))

(defun james/org-image-migrate--available-name (directory filename occupied)
  "Return an unused FILENAME in DIRECTORY according to OCCUPIED paths."
  (let* ((extension (file-name-extension filename t))
         (stem (file-name-sans-extension filename))
         (candidate filename)
         (suffix 2))
    (while (gethash (expand-file-name candidate directory) occupied)
      (setq candidate (format "%s-%d%s" stem suffix (or extension ""))
            suffix (1+ suffix)))
    candidate))

(defun james/org-image-migrate--write-cache (directory hash text)
  "Write OCR TEXT under HASH in cache DIRECTORY and return its basename."
  (make-directory directory t)
  (let ((file (expand-file-name (concat hash ".txt") directory)))
    (with-temp-file file
      (insert text)
      (unless (bolp) (insert "\n")))
    (file-name-nondirectory file)))

(defun james/org-image-migrate--scan-org-file
    (org-file root cache-directory occupied seen)
  "Return migration rows for ORG-FILE beneath ROOT.
CACHE-DIRECTORY stores new OCR, OCCUPIED reserves targets, and SEEN rejects
images referenced more than once."
  (let ((org-hash (james/org-image-migrate--hash-file org-file)) rows)
    (with-temp-buffer
      (insert-file-contents org-file)
      (setq-local buffer-file-name org-file
                  default-directory (file-name-directory org-file))
      (delay-mode-hooks (org-mode))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (equal (org-element-property :type link) "file")
            (let* ((path (org-link-unescape
                          (or (org-element-property :path link) "")))
                   (image (expand-file-name path default-directory))
                   (images-root (expand-file-name "images" root)))
              (when (and (james/org-image-file-p image)
                         (james/org-image-migrate--under-root-p
                          image images-root))
                (unless (file-regular-p image)
                  (error "Missing linked image: %s (%s)" image org-file))
                (when (gethash image seen)
                  (error "Image has multiple Org references: %s" image))
                (puthash image t seen)
                (let* ((image-hash (james/org-image-migrate--hash-file image))
                       (end (- (org-element-property :end link)
                               (org-element-property :post-blank link)))
                       (drawer (james/org-image-migrate--drawer-after end))
                       (stored (james/org-image-migrate--drawer-text
                                drawer image-hash))
                       (ocr-source (if stored "drawer" "generated"))
                       (ocr-text (or stored
                                     (james/org-image-migrate--run-ocr image)))
                       (cache (unless stored
                                (james/org-image-migrate--write-cache
                                 cache-directory image-hash ocr-text)))
                       (title-data
                        (james/org-image-migrate--run-title-ocr image))
                       (title (plist-get title-data :title))
                       (warning (if title-data
                                    (james/org-image-migrate--title-warning title)
                                  "no-layout-title"))
                       (timestamp (james/org-image-migrate--timestamp image))
                       (extension (file-name-extension image))
                       new-file action)
                  (unless timestamp
                    (error "Unrecognized image timestamp: %s" image))
                  (if (and title (string-match-p "[[:alnum:]]" title))
                      (let* ((base (james/org-image--sanitized-name
                                    (concat title "." extension)))
                             (desired (concat timestamp "-" base)))
                        (setq new-file
                              (if (equal desired (file-name-nondirectory image))
                                  image
                                (expand-file-name
                                 (james/org-image-migrate--available-name
                                  (file-name-directory image) desired occupied)
                                 (file-name-directory image)))
                              action (cond
                                      (warning "review")
                                      ((equal image new-file) "unchanged")
                                      (t "rename")))
                        (puthash new-file t occupied))
                    (setq new-file image action "review"))
                  (push
                   (james/org-image-migrate--make-row
                    :action action
                    :org-file (james/org-image-migrate--relative org-file root)
                    :line (line-number-at-pos (org-element-property :begin link))
                    :old-path (james/org-image-migrate--relative image root)
                    :new-path (james/org-image-migrate--relative new-file root)
                    :image-hash image-hash :org-hash org-hash
                    :ocr-source ocr-source :ocr-title (or title "")
                    :warning (or warning "")
                    :cache (or cache ""))
                   rows)))))))
      (nreverse rows))))

(defun james/org-image-migrate--field (value)
  "Return VALUE as a single safe TSV field."
  (replace-regexp-in-string "[\t\r\n]+" " " (format "%s" (or value ""))))

(defun james/org-image-migrate--decode-field (field)
  "Decode spreadsheet-style quoting in TSV FIELD."
  (if (and (>= (length field) 2)
           (string-prefix-p "\"" field)
           (string-suffix-p "\"" field))
      (replace-regexp-in-string
       "\"\"" "\"" (substring field 1 -1) t t)
    field))

(defun james/org-image-migrate--write-manifest
    (manifest root rows image-count)
  "Write MANIFEST for ROOT containing ROWS and IMAGE-COUNT metadata."
  (make-directory (file-name-directory (expand-file-name manifest)) t)
  (with-temp-file manifest
    (insert "# org-image-migration\t" james/org-image-migrate-manifest-version "\n"
            "# root\t" (expand-file-name root) "\n"
            "# image-count\t" (number-to-string image-count) "\n"
            (mapconcat #'identity
                       '("action" "org_file" "line" "old_path" "new_path"
                         "image_sha256" "org_sha256" "ocr_source"
                         "ocr_title" "warning" "ocr_cache")
                       "\t")
            "\n")
    (dolist (row rows)
      (insert
       (mapconcat
        #'james/org-image-migrate--field
        (list (james/org-image-migrate-row-action row)
              (james/org-image-migrate-row-org-file row)
              (james/org-image-migrate-row-line row)
              (james/org-image-migrate-row-old-path row)
              (james/org-image-migrate-row-new-path row)
              (james/org-image-migrate-row-image-hash row)
              (james/org-image-migrate-row-org-hash row)
              (james/org-image-migrate-row-ocr-source row)
              (james/org-image-migrate-row-ocr-title row)
              (james/org-image-migrate-row-warning row)
              (james/org-image-migrate-row-cache row))
        "\t")
       "\n"))))

(defun james/org-image-migrate--write-orphans (manifest root files)
  "Write FILES and hashes to MANIFEST's orphan inventory for ROOT."
  (with-temp-file (concat manifest ".orphans.tsv")
    (insert "path\tsha256\n")
    (dolist (file files)
      (insert (james/org-image-migrate--relative file root) "\t"
              (james/org-image-migrate--hash-file file) "\n"))))

(defun james/org-image-migrate-plan (root manifest)
  "Build a reviewed migration MANIFEST for the Org collection at ROOT."
  (setq root (file-name-as-directory (expand-file-name root))
        manifest (expand-file-name manifest))
  (unless (file-directory-p root) (error "Org root does not exist: %s" root))
  (let* ((cache-directory (concat manifest ".ocr"))
         (images (james/org-image-migrate--image-files root))
         (occupied (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         rows)
    (when (file-directory-p cache-directory)
      (delete-directory cache-directory t))
    (dolist (image images) (puthash (expand-file-name image) t occupied))
    (dolist (org-file (directory-files-recursively root "\\.org\\'"))
      (unless (string-prefix-p ".#" (file-name-nondirectory org-file))
        (setq rows
              (nconc rows
                     (james/org-image-migrate--scan-org-file
                      org-file root cache-directory occupied seen)))))
    (james/org-image-migrate--write-manifest
     manifest root rows (length images))
    (james/org-image-migrate--write-orphans
     manifest root
     (cl-remove-if (lambda (image) (gethash (expand-file-name image) seen))
                   images))
    (message "Planned %d linked image(s): %d rename, %d unchanged, %d review"
             (length rows)
             (cl-count "rename" rows :test #'equal
                       :key #'james/org-image-migrate-row-action)
             (cl-count "unchanged" rows :test #'equal
                       :key #'james/org-image-migrate-row-action)
             (cl-count "review" rows :test #'equal
                       :key #'james/org-image-migrate-row-action))
    rows))

(defun james/org-image-migrate--read-manifest (manifest)
  "Read MANIFEST and return (METADATA . ROWS)."
  (let (metadata rows header)
    (with-temp-buffer
      (insert-file-contents manifest)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-remove-suffix
                     "\r"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (cond
           ((string-prefix-p "# " line)
            (let ((fields (mapcar #'james/org-image-migrate--decode-field
                                  (split-string (substring line 2) "\t"))))
              (push (cons (car fields) (cadr fields)) metadata)))
           ((not header)
            (setq header (mapcar #'james/org-image-migrate--decode-field
                                 (split-string line "\t"))))
           ((not (string-empty-p line))
            (let ((fields (mapcar #'james/org-image-migrate--decode-field
                                  (split-string line "\t" nil))))
              (unless (= (length fields) 11)
                (error "Malformed manifest line %d" (line-number-at-pos)))
              (push
               (james/org-image-migrate--make-row
                :action (nth 0 fields) :org-file (nth 1 fields)
                :line (string-to-number (nth 2 fields))
                :old-path (nth 3 fields) :new-path (nth 4 fields)
                :image-hash (nth 5 fields) :org-hash (nth 6 fields)
                :ocr-source (nth 7 fields) :ocr-title (nth 8 fields)
                :warning (nth 9 fields) :cache (nth 10 fields))
               rows)))))
        (forward-line 1)))
    (unless (equal (cdr (assoc "org-image-migration" metadata))
                   james/org-image-migrate-manifest-version)
      (error "Unsupported migration manifest version"))
    (cons metadata (nreverse rows))))

(defun james/org-image-migrate--validate-backup ()
  "Require the wrapper's verified backup archive when configured."
  (when james/org-image-migrate-require-backup
    (let ((archive (getenv "ORG_IMAGE_MIGRATION_BACKUP_ARCHIVE")))
      (unless (and archive (file-regular-p archive))
        (error "Apply requires a verified backup archive from the wrapper")))))

(defun james/org-image-migrate--validate-relative-path (path root)
  "Expand PATH beneath ROOT or signal an error."
  (let ((file (expand-file-name path root)))
    (unless (james/org-image-migrate--under-root-p
             file (expand-file-name "images" root))
      (error "Manifest image path escapes images directory: %s" path))
    file))

(defun james/org-image-migrate--manifest-root (metadata requested-root)
  "Validate METADATA's root against REQUESTED-ROOT and return it."
  (let ((root (file-name-as-directory
               (expand-file-name (or (cdr (assoc "root" metadata)) "")))))
    (unless (file-equal-p root (expand-file-name requested-root))
      (error "Manifest root %s does not match requested root %s"
             root requested-root))
    root))

(defun james/org-image-migrate--org-locks (root)
  "Return Emacs lockfiles for Org files beneath ROOT."
  (cl-remove-if-not
   (lambda (file)
     (and (string-prefix-p ".#" (file-name-nondirectory file))
          (string-suffix-p ".org" file t)))
   (directory-files-recursively root "\\.org\\'" nil nil t)))

(defun james/org-image-migrate--find-link (old-file)
  "Return the unique Org link element targeting OLD-FILE in this buffer."
  (let (matches)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (and (equal (org-element-property :type link) "file")
                   (equal (expand-file-name
                           (org-link-unescape
                            (or (org-element-property :path link) "")))
                          old-file))
          (push link matches))))
    (unless (= (length matches) 1)
      (error "Expected one link to %s in %s, found %d"
             old-file buffer-file-name (length matches)))
    (car matches)))

(defun james/org-image-migrate--cache-text (manifest row)
  "Return cached OCR text for ROW relative to MANIFEST, or nil."
  (unless (string-empty-p (james/org-image-migrate-row-cache row))
    (let ((file (expand-file-name
                 (james/org-image-migrate-row-cache row)
                 (concat manifest ".ocr/"))))
      (unless (file-regular-p file) (error "Missing OCR cache: %s" file))
      (with-temp-buffer
        (insert-file-contents file)
        (james/org-image-migrate--clean-text (buffer-string))))))

(defun james/org-image-migrate--new-link (link new-file)
  "Return LINK rewritten to NEW-FILE while preserving its description."
  (let* ((path (org-link-unescape (org-element-property :path link)))
         (target (if (file-name-absolute-p path)
                     new-file
                   (file-relative-name new-file default-directory)))
         (description
          (when-let* ((begin (org-element-property :contents-begin link))
                      (end (org-element-property :contents-end link)))
            (buffer-substring-no-properties begin end))))
    (org-link-make-string (concat "file:" (org-link-escape target)) description)))

(defun james/org-image-migrate--format-drawer (text hash indent)
  "Format OCR TEXT and HASH as a generated drawer using INDENT."
  (concat indent ":OCR:\n"
          indent ": sha256: " hash "\n"
          indent ": options: "
          (prin1-to-string james/org-image-migrate-ocr-arguments) "\n"
          (mapconcat (lambda (line) (concat indent ": " line "\n"))
                     (split-string text "\n" t) "")
          indent ":END:\n"))

(defun james/org-image-migrate--apply-row-to-buffer
    (row old-file new-file cache-text)
  "Apply ROW for OLD-FILE and NEW-FILE with optional CACHE-TEXT."
  (let* ((link (james/org-image-migrate--find-link old-file))
         (begin (org-element-property :begin link))
         (end (- (org-element-property :end link)
                 (org-element-property :post-blank link)))
         (drawer (james/org-image-migrate--drawer-after end))
         (indent (save-excursion
                   (goto-char begin)
                   (make-string (current-column) ?\s)))
         (new-link (james/org-image-migrate--new-link link new-file)))
    (when (and cache-text drawer)
      (delete-region (org-element-property :begin drawer)
                     (org-element-property :end drawer)))
    (goto-char begin)
    (delete-region begin end)
    (insert new-link)
    (when cache-text
      (end-of-line)
      (if (eobp) (insert "\n") (forward-char 1))
      (insert (james/org-image-migrate--format-drawer
               cache-text (james/org-image-migrate-row-image-hash row) indent)))))

(defun james/org-image-migrate--atomic-write (file contents)
  "Atomically replace FILE with CONTENTS while preserving its mode."
  (let ((temporary (make-temp-file
                    (expand-file-name ".org-image-migrate-"
                                      (file-name-directory file))))
        (mode (file-modes file)))
    (unwind-protect
        (progn
          (with-temp-file temporary (insert contents))
          (when mode (set-file-modes temporary mode))
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun james/org-image-migrate--write-journal (manifest state rows)
  "Write migration STATE and ROWS to MANIFEST's journal."
  (with-temp-file (concat manifest ".journal.tsv")
    (insert "state\t" state "\nold_path\tnew_path\tsha256\n")
    (dolist (row rows)
      (when (equal (james/org-image-migrate-row-action row) "rename")
        (insert (james/org-image-migrate-row-old-path row) "\t"
                (james/org-image-migrate-row-new-path row) "\t"
                (james/org-image-migrate-row-image-hash row) "\n")))))

(defun james/org-image-migrate-apply (requested-root manifest)
  "Apply reviewed MANIFEST transactionally beneath REQUESTED-ROOT."
  (james/org-image-migrate--validate-backup)
  (setq manifest (expand-file-name manifest))
  (pcase-let* ((`(,metadata . ,rows)
                (james/org-image-migrate--read-manifest manifest))
               (root (james/org-image-migrate--manifest-root
                      metadata requested-root))
               (review (cl-find "review" rows :test #'equal
                                :key #'james/org-image-migrate-row-action))
               (allowed '("rename" "unchanged" "skip")))
    (when-let* ((locks (james/org-image-migrate--org-locks root)))
      (error "Close Org buffers before applying; found lockfile: %s"
             (car locks)))
    (when review
      (error "Manifest still contains review rows"))
    (dolist (row rows)
      (unless (member (james/org-image-migrate-row-action row) allowed)
        (error "Unknown manifest action: %s"
               (james/org-image-migrate-row-action row))))
    (let ((org-data (make-hash-table :test #'equal))
          (targets (make-hash-table :test #'equal))
          staged finalized written)
      (condition-case err
          (progn
            (dolist (row rows)
              (let* ((org-file (expand-file-name
                                (james/org-image-migrate-row-org-file row) root))
                     (old-file (james/org-image-migrate--validate-relative-path
                                (james/org-image-migrate-row-old-path row) root))
                     (new-file (james/org-image-migrate--validate-relative-path
                                (james/org-image-migrate-row-new-path row) root))
                     (action (james/org-image-migrate-row-action row)))
                (unless (and (james/org-image-migrate--under-root-p org-file root)
                             (string-suffix-p ".org" org-file t))
                  (error "Manifest Org path escapes collection: %s" org-file))
                (unless (equal (james/org-image-migrate--hash-file org-file)
                               (james/org-image-migrate-row-org-hash row))
                  (error "Org file changed after planning: %s" org-file))
                (unless (and (file-regular-p old-file)
                             (equal (james/org-image-migrate--hash-file old-file)
                                    (james/org-image-migrate-row-image-hash row)))
                  (error "Image changed after planning: %s" old-file))
                (when (equal action "rename")
                  (unless (equal (file-name-directory old-file)
                                 (file-name-directory new-file))
                    (error "Migration cannot move images between directories: %s"
                           new-file))
                  (unless (equal (downcase (or (file-name-extension old-file) ""))
                                 (downcase (or (file-name-extension new-file) "")))
                    (error "Migration cannot change image extension: %s" new-file))
                  (unless (string-match-p
                           "\\`[0-9]\\{12\\}-[[:alnum:]].*\\.[[:alnum:]]+\\'"
                           (file-name-nondirectory new-file))
                    (error "Invalid reviewed target name: %s" new-file))
                  (when (gethash new-file targets)
                    (error "Duplicate migration target: %s" new-file))
                  (puthash new-file t targets)
                  (when (and (file-exists-p new-file)
                             (not (equal old-file new-file)))
                    (error "Migration target already exists: %s" new-file)))
                (unless (gethash org-file org-data)
                  (puthash org-file
                           (list :original
                                 (with-temp-buffer
                                   (insert-file-contents org-file)
                                   (buffer-string))
                                 :rows nil)
                           org-data))
                (push (list row old-file
                            (if (equal action "rename") new-file old-file)
                            (james/org-image-migrate--cache-text manifest row))
                      (plist-get (gethash org-file org-data) :rows))))
            ;; Prepare all Org changes in memory before touching the filesystem.
            (maphash
             (lambda (org-file data)
               (with-temp-buffer
                 (insert (plist-get data :original))
                 (setq-local buffer-file-name org-file
                             default-directory (file-name-directory org-file))
                 (delay-mode-hooks (org-mode))
                 (dolist (operation
                          (sort (plist-get data :rows)
                                (lambda (a b)
                                  (> (james/org-image-migrate-row-line (car a))
                                     (james/org-image-migrate-row-line (car b))))))
                   (james/org-image-migrate--apply-row-to-buffer
                    (nth 0 operation) (nth 1 operation) (nth 2 operation)
                    (nth 3 operation)))
                 (plist-put data :updated (buffer-string))))
             org-data)
            (james/org-image-migrate--write-journal manifest "prepared" rows)
            ;; Stage every source first so no partial target set can collide.
            (dolist (row rows)
              (when (equal (james/org-image-migrate-row-action row) "rename")
                (let* ((old (expand-file-name
                             (james/org-image-migrate-row-old-path row) root))
                       (temporary
                        (make-temp-file
                         (expand-file-name ".org-image-migrate-"
                                           (file-name-directory old)))))
                  (delete-file temporary)
                  (rename-file old temporary)
                  (push (cons old temporary) staged))))
            (dolist (pair staged)
              (let* ((old (car pair))
                     (row (cl-find old rows :test #'equal
                                   :key (lambda (candidate)
                                          (expand-file-name
                                           (james/org-image-migrate-row-old-path
                                            candidate)
                                           root))))
                     (new (expand-file-name
                           (james/org-image-migrate-row-new-path row) root)))
                (rename-file (cdr pair) new)
                (push (cons old new) finalized)))
            (maphash
             (lambda (org-file data)
               (james/org-image-migrate--atomic-write
                org-file (plist-get data :updated))
               (push org-file written))
             org-data)
            (james/org-image-migrate--write-journal manifest "applied" rows)
            (james/org-image-migrate-verify root manifest)
            (message "Applied %d image migration row(s)" (length rows)))
        (error
         (dolist (org-file written)
           (james/org-image-migrate--atomic-write
            org-file (plist-get (gethash org-file org-data) :original)))
         (dolist (pair finalized)
           (when (file-exists-p (cdr pair))
             (rename-file (cdr pair) (car pair) t)))
         (dolist (pair staged)
           (when (file-exists-p (cdr pair))
             (rename-file (cdr pair) (car pair) t)))
         (james/org-image-migrate--write-journal manifest "rolled-back" rows)
         (signal (car err) (cdr err)))))))

(defun james/org-image-migrate--verify-orphans (manifest root)
  "Verify MANIFEST's orphan inventory beneath ROOT."
  (let ((file (concat manifest ".orphans.tsv")))
    (unless (file-regular-p file) (error "Missing orphan inventory: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (forward-line 1)
      (while (not (eobp))
        (let* ((fields (split-string
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)) "\t"))
               (image (expand-file-name (car fields) root)))
          (unless (and (file-regular-p image)
                       (equal (james/org-image-migrate--hash-file image)
                              (cadr fields)))
            (error "Orphan image changed: %s" image)))
        (forward-line 1)))))

(defun james/org-image-migrate-verify (requested-root manifest)
  "Verify an applied MANIFEST beneath REQUESTED-ROOT."
  (setq manifest (expand-file-name manifest))
  (pcase-let* ((`(,metadata . ,rows)
                (james/org-image-migrate--read-manifest manifest))
               (root (james/org-image-migrate--manifest-root
                      metadata requested-root))
               (expected-count
                (string-to-number (or (cdr (assoc "image-count" metadata)) "0")))
               (actual-count (length (james/org-image-migrate--image-files root))))
    (unless (= expected-count actual-count)
      (error "Image count changed: expected %d, found %d"
             expected-count actual-count))
    (dolist (row rows)
      (when (equal (james/org-image-migrate-row-action row) "review")
        (error "Manifest still contains review rows"))
      (let* ((renamed (equal (james/org-image-migrate-row-action row) "rename"))
             (relative (if renamed
                           (james/org-image-migrate-row-new-path row)
                         (james/org-image-migrate-row-old-path row)))
             (image (expand-file-name relative root))
             (org-file (expand-file-name
                        (james/org-image-migrate-row-org-file row) root)))
        (unless (and (file-regular-p image)
                     (equal (james/org-image-migrate--hash-file image)
                            (james/org-image-migrate-row-image-hash row)))
          (error "Migrated image missing or changed: %s" image))
        (when (and renamed
                   (not (equal (james/org-image-migrate-row-old-path row)
                               (james/org-image-migrate-row-new-path row)))
                   (file-exists-p
                    (expand-file-name
                     (james/org-image-migrate-row-old-path row) root)))
          (error "Old image still exists: %s"
                 (james/org-image-migrate-row-old-path row)))
        (with-temp-buffer
          (insert-file-contents org-file)
          (setq-local buffer-file-name org-file
                      default-directory (file-name-directory org-file))
          (delay-mode-hooks (org-mode))
          (james/org-image-migrate--find-link image))))
    (james/org-image-migrate--verify-orphans manifest root)
    ;; Every image link in the collection must still resolve.
    (dolist (org-file (directory-files-recursively root "\\.org\\'"))
      (with-temp-buffer
        (insert-file-contents org-file)
        (setq-local default-directory (file-name-directory org-file))
        (delay-mode-hooks (org-mode))
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (and (equal (org-element-property :type link) "file")
                       (james/org-image-file-p
                        (org-element-property :path link)))
              (let ((file (expand-file-name
                           (org-link-unescape
                            (org-element-property :path link)))))
                (unless (file-regular-p file)
                  (error "Broken image link in %s: %s" org-file file))))))))
    (message "Verified %d image migration row(s)" (length rows))
    t))

(defun james/org-image-migrate-batch ()
  "Batch entry point for plan, apply, and verify migration commands."
  (when (equal (car command-line-args-left) "--")
    (pop command-line-args-left))
  (let* ((command (pop command-line-args-left))
         (manifest (pop command-line-args-left))
         (root (getenv "ORG_SOURCE_DIR")))
    (unless (and command manifest root (null command-line-args-left))
      (error "Usage: migrate-org-images.sh plan|apply|verify MANIFEST"))
    (pcase command
      ("plan" (james/org-image-migrate-plan root manifest))
      ("apply" (james/org-image-migrate-apply root manifest))
      ("verify" (james/org-image-migrate-verify root manifest))
      (_ (error "Unknown migration command: %s" command)))))

(provide 'james-org-image-migrate)
;;; james-org-image-migrate.el ends here
