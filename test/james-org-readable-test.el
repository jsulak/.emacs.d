;;; james-org-readable-test.el --- Readable publication tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'james-org-readable)

(ert-deftest james/org-readable-preserves-structure-links-and-hides-drawers ()
  (with-temp-buffer
    (insert "#+TITLE: Example\n* TODO 🚩 Heading\n:PROPERTIES:\n:ID: internal-id\n:END:\n:OCR:\nHidden OCR text\n:END:\n:LOGBOOK:\nHidden log text\n:END:\n*bold* and /italic/\n- [ ] Task\n[[https://example.com/path?q=a&b=c][External link]]\n[[file:attachments/report.pdf][Report]]\n")
    (delay-mode-hooks (org-mode))
    (let ((tex (james/org-readable--latex "example.org")))
      (dolist (expected '("\\section*{" "\\addcontentsline{toc}{section}"
                           "\\textbf{bold}" "\\emph{italic}"
                           "\\href{https://example.com/path?q=a\\&b=c}{External link}"
                           "Report (\\texorpdfstring{\\nolinkurl{attachments/report.pdf}}{attachments/report.pdf})"
                           "[!]" "texgyreheros"))
        (should (string-search expected tex)))
      (dolist (hidden '("Hidden OCR" "Hidden log" "internal-id" "🚩"))
        (should-not (string-search hidden tex))))))

(ert-deftest james/org-readable-url-labels-use-breakable-url-rendering ()
  (with-temp-buffer
    (insert "[[https://example.com/a-long-path][https://example.com/a-long-path]]\n")
    (delay-mode-hooks (org-mode))
    (let ((tex (james/org-readable--latex "example.org")))
      (should (string-search "\\url{https://example.com/a-long-path}" tex))
      (should-not (string-search "\\href{https://example.com/a-long-path}" tex)))))

(ert-deftest james/org-readable-attachment-labels-break-at-underscores ()
  (with-temp-buffer
    (insert "[[file:attachments/report.pptx][Long_Report_Filename.pptx]]\n")
    (delay-mode-hooks (org-mode))
    (let ((tex (james/org-readable--latex "example.org")))
      (should (string-search "Long\\_\\allowbreak{}Report\\_\\allowbreak{}Filename.pptx" tex)))))

(ert-deftest james/org-readable-heading-links-use-readable-translator ()
  (with-temp-buffer
    (insert "* 🚩 [[file:other.org][Other note]]\n")
    (delay-mode-hooks (org-mode))
    (let ((tex (james/org-readable--latex "example.org")))
      (should (string-search "Other note (\\texorpdfstring{\\nolinkurl{other.org}}{other.org})" tex))
      (should-not (string-search "\\href{other.org}" tex))
      (should-not (string-search "🚩" tex)))))

(ert-deftest james/org-readable-babel-is-not-executed ()
  (with-temp-buffer
    (insert "#+begin_src emacs-lisp :exports results\n(error \"Should not run\")\n#+end_src\n")
    (delay-mode-hooks (org-mode))
    (cl-letf (((symbol-function 'org-babel-execute-src-block)
               (lambda (&rest _) (ert-fail "Babel executed during publication"))))
      (should (stringp (james/org-readable--latex "example.org"))))))

(ert-deftest james/org-readable-wraps-wide-tables-on-landscape-pages ()
  (with-temp-buffer
    (insert "| A | B | C | D |\n|---+---+---+---|\n| one | two | three | four |\n")
    (delay-mode-hooks (org-mode))
    (let ((tex (james/org-readable--latex "example.org")))
      (should (string-search "\\begin{landscape}" tex))
      (should (string-search "\\begin{longtable}" tex))
      (should (string-search "\\linewidth/4" tex)))))

(ert-deftest james/org-readable-deep-lists-keep-items-with-bounded-nesting ()
  (with-temp-buffer
    (insert "- one\n  - two\n    - three\n      - four\n        - five\n          - six\n")
    (delay-mode-hooks (org-mode))
    (let ((tex (james/org-readable--latex "example.org")))
      (should (= 3 (with-temp-buffer
                     (insert tex)
                     (goto-char (point-min))
                     (let ((count 0))
                       (while (search-forward "\\begin{itemize}" nil t)
                         (cl-incf count))
                       count))))
      (dolist (item '("one" "two" "three" "four" "five" "six"))
        (should (string-search (concat "\\item " item) tex))))))

(ert-deftest james/org-readable-fingerprint-tracks-the-captured-buffer ()
  (let ((source (make-temp-file "readable-source-" nil ".org" "* Saved note\n")))
    (unwind-protect
        (with-temp-buffer
          (insert "* Captured note\n")
          (delay-mode-hooks (org-mode))
          (let ((fingerprint (james/org-readable--fingerprint source "style")))
            (with-temp-file source (insert "* Concurrent save\n"))
            (should (equal fingerprint (james/org-readable--fingerprint source "style")))
            (goto-char (point-max))
            (insert "New content\n")
            (should-not (equal fingerprint (james/org-readable--fingerprint source "style")))))
      (delete-file source))))

(ert-deftest james/org-readable-rejects-escaping-output-paths ()
  (let ((root (make-temp-file "readable-path-test-" t)))
    (unwind-protect
        (progn
          (should-error (james/org-readable--output "../outside.org" root))
          (should-error (james/org-readable--output "/absolute.org" root))
          (make-symbolic-link temporary-file-directory (expand-file-name "nested" root))
          (should-error (james/org-readable--output "nested/note.org" root)))
      (delete-directory root t))))

(ert-deftest james/org-readable-incremental-images-failure-retry-and-pruning ()
  (let* ((root (make-temp-file "readable-publish-test-" t))
         (source (expand-file-name "source" root))
         (destination (expand-file-name "readable" root))
         (state (expand-file-name "state" root))
         (note (expand-file-name "note.org" source))
         (image (expand-file-name "image.png" source))
         (output (expand-file-name "note.pdf" destination))
         (calls 0)
         fail)
    (unwind-protect
        (progn
          (make-directory source)
          (with-temp-file note (insert "* Note\n[[file:image.png]]\n"))
          (with-temp-file image (insert "image one"))
          (cl-letf (((symbol-function 'executable-find) (lambda (_) "/test/xelatex"))
                    ((symbol-function 'james/org-readable--publish)
                     (lambda (_source target _log)
                       (cl-incf calls)
                       (when fail (error "Simulated compilation failure"))
                       (with-temp-file target (insert (format "PDF version %d" calls))))))
            (should (zerop (james/org-readable-export source destination state)))
            (should (= calls 1))
            (should (zerop (james/org-readable-export source destination state)))
            (should (= calls 1))
            ;; Content changes with an unchanged image mtime must still rebuild.
            (let ((mtime (file-attribute-modification-time (file-attributes image))))
              (with-temp-file image (insert "image two"))
              (set-file-times image mtime))
            (should (zerop (james/org-readable-export source destination state)))
            (should (= calls 2))
            (with-temp-file note (insert "* Changed note\n[[file:image.png]]\n"))
            (setq fail t)
            (should (= 1 (james/org-readable-export source destination state)))
            (should (equal (with-temp-buffer (insert-file-contents output) (buffer-string))
                           "PDF version 2"))
            (setq fail nil)
            (should (zerop (james/org-readable-export source destination state)))
            (should (= calls 4))
            ;; Missing output is repaired even if source and dependencies match.
            (delete-file output)
            (should (zerop (james/org-readable-export source destination state)))
            (should (= calls 5))
            ;; Partial sample runs must not prune the rest of the collection.
            (with-temp-file (expand-file-name "other.org" source) (insert "* Other"))
            (should (zerop (james/org-readable-export source destination state '("other.org"))))
            (should (file-exists-p output))
            (with-temp-file (expand-file-name "unmanaged.pdf" destination) (insert "keep"))
            (delete-file note)
            (should (zerop (james/org-readable-export source destination state)))
            (should-not (file-exists-p output))
            (should (file-exists-p (expand-file-name "unmanaged.pdf" destination)))))
      (delete-directory root t))))

;;; james-org-readable-test.el ends here
