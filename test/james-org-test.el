;;; james-org-test.el --- Tests for Org configuration -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-download)

(defun james-test/call-with-org-file (relative-path function)
  "Call FUNCTION with a temporary Org collection and RELATIVE-PATH file.
FUNCTION receives the collection root and the absolute Org filename."
  (let* ((root (make-temp-file "james-org-test-" t))
         (org-directory root)
         (org-file (expand-file-name relative-path root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory org-file) t)
          (with-temp-file org-file)
          (with-temp-buffer
            (setq buffer-file-name org-file
                  default-directory (file-name-directory org-file))
            (org-mode)
            (funcall function root org-file)))
      (delete-directory root t))))

(defun james-test/org-file-application (filename)
  "Return the configured Org application for FILENAME."
  (let* ((apps (org--file-apps-regexp-alist org-file-apps))
         (extension (file-name-extension filename)))
    (or (assoc-default (downcase filename) apps #'string-match-p)
        (cdr (assoc extension org-file-apps))
        (cdr (assq t org-file-apps)))))

(ert-deftest james/org-tags-follow-heading-text ()
  (should (zerop org-tags-column))
  (with-temp-buffer
    (insert "* Heading                                      :example:\n")
    (org-mode)
    (goto-char (point-min))
    (org-align-tags)
    (should (equal (buffer-string) "* Heading :example:\n"))))

(ert-deftest james/org-attachments-open-with-system-application ()
  (dolist (filename '("report.pdf" "presentation.pptx" "document.docx"
                      "workbook.xlsx" "archive.zip" "notes.md"))
    (should (string-prefix-p "open "
                             (james-test/org-file-application filename))))
  (should (eq (james-test/org-file-application "related.org") 'emacs))
  (should (eq (james-test/org-file-application "history.org_archive")
              'emacs)))

(ert-deftest james/org-image-directory-for-root-note ()
  (james-test/call-with-org-file
   "meeting-notes.org"
   (lambda (root _org-file)
     (should
      (equal (james/org-image-directory)
             (expand-file-name "images/meeting-notes" root))))))

(ert-deftest james/org-image-directory-for-nested-note ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (root _org-file)
     (should
      (equal (james/org-image-directory)
             (expand-file-name "images/projects/alpha" root))))))

(ert-deftest james/org-attachment-directory-for-nested-note ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (root _org-file)
     (should
      (equal (james/org-attachment-directory)
             (expand-file-name "attachments/projects/alpha" root))))))

(ert-deftest james/org-attachment-directory-supports-relative-root ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (root _org-file)
     (let ((james/org-attachment-root "files"))
       (should
        (equal (james/org-attachment-directory)
               (expand-file-name "files/projects/alpha" root)))))))

(ert-deftest james/org-attachment-directory-supports-external-root ()
  (let ((attachment-root (make-temp-file "james-org-attachments-" t)))
    (unwind-protect
        (james-test/call-with-org-file
         "projects/alpha.org"
         (lambda (_root _org-file)
           (let ((james/org-attachment-root attachment-root))
             (should
              (equal (james/org-attachment-directory)
                     (expand-file-name "projects/alpha" attachment-root))))))
      (delete-directory attachment-root t))))

(ert-deftest james/org-image-directory-requires-collection-file ()
  (let ((org-directory (make-temp-file "james-org-root-" t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (should-error (james/org-image-directory) :type 'user-error))
          (let ((buffer-file-name
                 (expand-file-name "outside.org"
                                   (file-name-directory org-directory))))
            (should-error (james/org-image-directory) :type 'user-error)))
      (delete-directory org-directory t))))

(ert-deftest james/org-image-sanitizes-source-names ()
  (should (equal (james/org-image--sanitized-name "My Diagram (Final).PNG")
                 "my-diagram-final.png"))
  (should (equal (james/org-image--sanitized-name "  !!!.JPG")
                 "image.jpg")))

(ert-deftest james/org-link-description-adds-document-icons ()
  (dolist (case '(("Quarterly Results.PPTX" "Results" "📊 Results")
                  ("Proposal.docx" "Proposal" "📄 Proposal")
                  ("Forecast.XLSX" "Forecast" "📈 Forecast")
                  ("https://example.test/brief.PDF?download=1"
                   "Brief" "📕 Brief")
                  ("notes.txt" "Notes" "📎 Notes")
                  ("https://example.test" "Example" "Example")
                  ("photo.png" "Photo" "Photo")))
    (should (equal (james/org--link-description (nth 0 case) (nth 1 case))
                   (nth 2 case))))
  (should (equal (james/org--link-description
                  "archive.zip" "Archive.zip" t)
                 "📎 Archive.zip")))

(ert-deftest james/org-timestamp-prefix-is-compact-to-the-minute ()
  (cl-letf (((symbol-function 'format-time-string)
             (lambda (format &rest _)
               (should (equal format "%Y%m%d%H%M-"))
               "202608230953-")))
    (should (equal (james/org--timestamp-prefix) "202608230953-"))))

(ert-deftest james/org-image-filename-adds-collision-suffixes ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (_root _org-file)
     (let ((directory (james/org-image-directory)))
       (make-directory directory t)
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "202607181435-")))
         (should
          (equal (james/org-download-file-name "System Architecture.PNG")
                 "202607181435-system-architecture.png"))
         (with-temp-file
             (expand-file-name
              "202607181435-system-architecture.png" directory))
         (should
          (equal (james/org-download-file-name "System Architecture.PNG")
                 "202607181435-system-architecture-2.png"))
         (with-temp-file
             (expand-file-name
              "202607181435-system-architecture-2.png" directory))
         (should
          (equal (james/org-download-file-name "System Architecture.PNG")
                 "202607181435-system-architecture-3.png")))))))

(ert-deftest james/org-attachment-insert-copies-and-links-file ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (root _org-file)
     (let* ((source-directory (expand-file-name "incoming" root))
            (source (expand-file-name "Design Review (Final).PDF"
                                      source-directory))
            (target-directory
             (expand-file-name "attachments/projects/alpha" root))
            (target-name "202607181435-design-review-final.pdf")
            (target (expand-file-name target-name target-directory)))
       (make-directory source-directory t)
       (with-temp-file source (insert "pdf data"))
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "202607181435-")))
         (should (equal (james/org-attachment-insert source) target)))
       (should (file-exists-p source))
       (should (equal (with-temp-buffer
                        (insert-file-contents target)
                        (buffer-string))
                      "pdf data"))
       (should
        (string-match-p
         (regexp-quote
           (format
            "[[file:../attachments/projects/alpha/%s][📕 Design Review (Final).PDF]]"
            target-name))
          (buffer-string)))
       (should-not (string-match-p ":PROPERTIES:" (buffer-string)))))))

(ert-deftest james/org-attachment-insert-uses-external-root ()
  (let ((attachment-root (make-temp-file "james-org-attachments-" t)))
    (unwind-protect
        (james-test/call-with-org-file
         "projects/alpha.org"
         (lambda (root org-file)
           (let* ((james/org-attachment-root attachment-root)
                  (source (expand-file-name "incoming/Agenda.PDF" root))
                  (target-name "202607181435-agenda.pdf")
                  (target (expand-file-name
                           (concat "projects/alpha/" target-name)
                           attachment-root)))
             (make-directory (file-name-directory source) t)
             (with-temp-file source (insert "agenda"))
             (cl-letf (((symbol-function 'format-time-string)
                        (lambda (&rest _) "202607181435-")))
               (should (equal (james/org-attachment-insert source) target)))
             (should (file-exists-p target))
             (should
              (string-match-p
               (regexp-quote
                (format "[[file:%s][📕 Agenda.PDF]]"
                        (org-link-escape
                         (file-relative-name
                          target (file-name-directory org-file)))))
               (buffer-string))))))
      (delete-directory attachment-root t))))

(ert-deftest james/org-drag-drop-routes-attachments-and-images ()
  (james-test/call-with-org-file
   "notes/example.org"
   (lambda (root _org-file)
     (let* ((source-directory (expand-file-name "incoming" root))
            (attachment (expand-file-name "Agenda.PDF" source-directory))
            (image (expand-file-name "Diagram.PNG" source-directory))
            (fallback-calls nil))
       (make-directory source-directory t)
       (with-temp-file attachment (insert "attachment"))
       (with-temp-file image (insert "image"))
       (cl-labels ((fallback (uri action)
                     (push (list uri action) fallback-calls)
                     'fallback))
         (cl-letf (((symbol-function 'format-time-string)
                    (lambda (&rest _) "202607181435-"))
                   ((symbol-function 'url-copy-file)
                    (lambda (_uri target &optional _ok-if-exists)
                      (with-temp-file target (insert "downloaded")))))
           (should
            (eq (james/org-download-dnd-with-attachments
                 #'fallback (concat "file://" attachment) 'copy)
                'copy))
           (should
            (eq (james/org-download-dnd-with-attachments
                 #'fallback "https://example.test/Board%20Packet.PDF" 'copy)
                'copy)))
         (should
          (eq (james/org-download-dnd-with-attachments
               #'fallback (concat "file://" image) 'copy)
              'fallback)))
       (should (= (length fallback-calls) 1))
       (should
        (file-exists-p
         (expand-file-name
          "attachments/notes/example/202607181435-agenda.pdf" root)))
       (should
        (file-exists-p
         (expand-file-name
          "attachments/notes/example/202607181435-board-packet.pdf" root)))
       (should
        (string-match-p
         (regexp-quote
          "[[file:../attachments/notes/example/202607181435-agenda.pdf][📕 Agenda.PDF]]")
         (buffer-string)))
       (should
        (string-match-p
         (regexp-quote
          "[[file:../attachments/notes/example/202607181435-board-packet.pdf][📕 Board Packet.PDF]]")
         (buffer-string)))))))

(ert-deftest james/org-download-clipboard-uses-clean-name-without-id ()
  (let (received-name id-result)
    (james/org-download-clipboard-without-id-property
     (lambda (&optional basename)
       (setq received-name basename
             id-result (org-id-get-create))))
    (should (equal received-name "clipboard.png"))
    (should-not id-result)))

(ert-deftest james/org-download-clipboard-copies-and-links-image ()
  (james-test/call-with-org-file
   "notes/clipboard.org"
   (lambda (root _org-file)
     (let* ((source-directory (expand-file-name "incoming" root))
            (target-directory (expand-file-name "images/notes/clipboard" root))
            (target-name "202607181435-clipboard.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil)
            (id-calls 0))
       (make-directory source-directory t)
       (insert "* Clipboard\n")
       (cl-letf (((symbol-function 'executable-find)
                  (lambda (_) "/usr/local/bin/pngpaste"))
                 ((symbol-function 'format-time-string)
                  (lambda (&rest _) "202607181435-"))
                 ((symbol-function 'org-id-get-create)
                  (lambda () (setq id-calls (1+ id-calls))))
                 ((symbol-function 'org-download-screenshot)
                  (lambda (&optional basename)
                    (let ((source (expand-file-name basename source-directory)))
                      (with-temp-file source (insert "clipboard image"))
                      (org-download-image source)))))
         (org-download-clipboard))
       (should (= id-calls 0))
       (should (file-exists-p target))
       (should
        (string-match-p
         (regexp-quote
          (format "[[file:../images/notes/clipboard/%s]]" target-name))
         (buffer-string)))
       (should-not (string-match-p ":PROPERTIES:" (buffer-string)))))))

(ert-deftest james/org-download-drag-drop-copies-and-links-image ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (root _org-file)
     (let* ((source-directory (expand-file-name "incoming" root))
            (source (expand-file-name "System Architecture (Final).PNG"
                                      source-directory))
            (target-directory (expand-file-name "images/projects/alpha" root))
            (target-name
             "202607181435-system-architecture-final.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil))
       (make-directory source-directory t)
       (with-temp-file source (insert "image data"))
       (insert "* Diagram\n")
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "202607181435-")))
         (org-download-image (concat "file://" source)))
       (should (file-exists-p target))
       (should (equal (with-temp-buffer
                        (insert-file-contents target)
                        (buffer-string))
                      "image data"))
       (should
        (string-match-p
         (regexp-quote
          (format "[[file:../images/projects/alpha/%s]]" target-name))
         (buffer-string)))
       (should-not (string-match-p ":PROPERTIES:" (buffer-string)))))))

(ert-deftest james/org-download-screenshot-copies-and-links-image ()
  (james-test/call-with-org-file
   "notes/screenshots.org"
   (lambda (root _org-file)
     (let* ((capture-file (expand-file-name "capture/screenshot.png" root))
            (target-directory (expand-file-name "images/notes/screenshots" root))
            (target-name "202607181435-screenshot.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil)
            (org-download-screenshot-file capture-file)
            (org-download-screenshot-method
             (lambda (filename)
               (with-temp-file filename (insert "screenshot image")))))
       (insert "* Screenshot\n")
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "202607181435-")))
         (org-download-screenshot))
       (should (file-exists-p target))
       (should-not (file-exists-p capture-file))
       (should
        (string-match-p
         (regexp-quote
          (format "[[file:../images/notes/screenshots/%s]]" target-name))
         (buffer-string)))))))

(ert-deftest james/org-download-base64-drag-drop-uses-image-name ()
  (james-test/call-with-org-file
   "notes/example.org"
   (lambda (root _org-file)
     (let* ((target-directory (expand-file-name "images/notes/example" root))
            (target-name "202607181435-image.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil))
       (insert "* Image\n")
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "202607181435-")))
         (org-download-dnd-base64
          "data:image/png;base64,aGVsbG8gd29ybGQ=" nil))
       (should (file-exists-p target))
       (should
        (string-match-p
         (regexp-quote
          (format "[[file:../images/notes/example/%s]]" target-name))
         (buffer-string)))))))

(ert-deftest james/org-download-directory-preserves-non-org-behavior ()
  (with-temp-buffer
    (setq major-mode 'dired-mode)
    (should (equal (james/org-download-directory (lambda () "/fallback"))
                   "/fallback"))))

;;; james-org-test.el ends here
