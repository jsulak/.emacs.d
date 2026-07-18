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

(ert-deftest james/org-image-filename-adds-collision-suffixes ()
  (james-test/call-with-org-file
   "projects/alpha.org"
   (lambda (_root _org-file)
     (let ((directory (james/org-image-directory)))
       (make-directory directory t)
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "20260718-143522-")))
         (should
          (equal (james/org-download-file-name "System Architecture.PNG")
                 "20260718-143522-system-architecture.png"))
         (with-temp-file
             (expand-file-name
              "20260718-143522-system-architecture.png" directory))
         (should
          (equal (james/org-download-file-name "System Architecture.PNG")
                 "20260718-143522-system-architecture-2.png"))
         (with-temp-file
             (expand-file-name
              "20260718-143522-system-architecture-2.png" directory))
         (should
          (equal (james/org-download-file-name "System Architecture.PNG")
                 "20260718-143522-system-architecture-3.png")))))))

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
            (target-name "20260718-143522-clipboard.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil)
            (id-calls 0))
       (make-directory source-directory t)
       (insert "* Clipboard\n")
       (cl-letf (((symbol-function 'executable-find)
                  (lambda (_) "/usr/local/bin/pngpaste"))
                 ((symbol-function 'format-time-string)
                  (lambda (&rest _) "20260718-143522-"))
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
             "20260718-143522-system-architecture-final.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil))
       (make-directory source-directory t)
       (with-temp-file source (insert "image data"))
       (insert "* Diagram\n")
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "20260718-143522-")))
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
            (target-name "20260718-143522-screenshot.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil)
            (org-download-screenshot-file capture-file)
            (org-download-screenshot-method
             (lambda (filename)
               (with-temp-file filename (insert "screenshot image")))))
       (insert "* Screenshot\n")
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "20260718-143522-")))
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
            (target-name "20260718-143522-image.png")
            (target (expand-file-name target-name target-directory))
            (org-download-display-inline-images nil))
       (insert "* Image\n")
       (cl-letf (((symbol-function 'format-time-string)
                  (lambda (&rest _) "20260718-143522-")))
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
