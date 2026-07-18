;;; james-functions-test.el --- Tests for editing utilities -*- lexical-binding: t; -*-

(require 'test-helper)

(ert-deftest james/smart-beginning-of-line-toggles-indentation-and-margin ()
  (with-temp-buffer
    (insert "    content")
    (goto-char (point-max))
    (james/smart-beginning-of-line)
    (should (= (current-column) 4))
    (james/smart-beginning-of-line)
    (should (bolp))))

(ert-deftest james/duplicate-line-duplicates-current-line ()
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (goto-char (point-min))
    (james/duplicate-line)
    (should (equal (buffer-string) "alpha\nalpha\nbeta\n"))))

(ert-deftest james/delete-enclosed-text-preserves-delimiters ()
  (with-temp-buffer
    (insert "before (inside) after")
    (search-backward "side")
    (james/delete-enclosed-text)
    (should (equal (buffer-string) "before () after"))))

(ert-deftest james/slick-copy-copies-current-line-without-region ()
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (goto-char (point-min))
    (let (received)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
        (james/slick-copy-advice
         (lambda (begin end &rest _)
           (setq received (buffer-substring begin end)))))
      (should (equal received "first line\n")))))

(ert-deftest james/slick-copy-preserves-explicit-region-arguments ()
  (let (received)
    (cl-letf (((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
      (james/slick-copy-advice
       (lambda (&rest args) (setq received args)) 3 8 'region))
    (should (equal received '(3 8 region)))))

(ert-deftest james/rename-file-and-buffer-renames-file-and-visit ()
  (james-test/with-temporary-directory directory
    (let* ((old-name (expand-file-name "old.txt" directory))
           (new-name (expand-file-name "new.txt" directory)))
      (with-temp-file old-name (insert "contents"))
      (with-temp-buffer
        (set-visited-file-name old-name)
        (cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest _) new-name)))
          (james/rename-file-and-buffer))
        (should (equal buffer-file-name new-name))
        (should-not (file-exists-p old-name))
        (should (file-exists-p new-name))))))

(ert-deftest james/move-buffer-file-moves-file-and-visit ()
  (james-test/with-temporary-directory directory
    (let* ((source-directory (expand-file-name "source" directory))
           (target-directory (expand-file-name "target" directory))
           (old-name (expand-file-name "note.txt" source-directory))
           (new-name (expand-file-name "note.txt" target-directory)))
      (make-directory source-directory)
      (make-directory target-directory)
      (with-temp-file old-name (insert "contents"))
      (with-temp-buffer
        (set-visited-file-name old-name)
        (james/move-buffer-file target-directory)
        (should (equal buffer-file-name new-name))
        (should-not (file-exists-p old-name))
        (should (file-exists-p new-name))))))

(ert-deftest james/file-rename-requires-an-existing-visited-file ()
  (with-temp-buffer
    (should-error (james/rename-file-and-buffer) :type 'user-error)
    (should-error (james/move-buffer-file temporary-file-directory)
                  :type 'user-error)))

(ert-deftest james/paste-markdown-as-org-cleans-converter-output ()
  (with-temp-buffer
    (let ((kill-ring '("# Heading\nBody")))
      (cl-letf (((symbol-function 'shell-command-on-region)
                 (lambda (_begin _end _command output-buffer &rest _)
                   (with-current-buffer output-buffer
                     (erase-buffer)
                     (insert "* Heading\nBody\n:PROPERTIES:\n:ID: 1\n:END:\n"))
                   0)))
        (james/paste-markdown-as-org))
      (should (equal (buffer-string) "* Heading\n\nBody\n")))))

(ert-deftest james/paste-markdown-as-org-reports-conversion-failure ()
  (with-temp-buffer
    (let ((kill-ring '("# Heading")))
      (cl-letf (((symbol-function 'shell-command-on-region)
                 (lambda (&rest _) 1)))
        (should-error (james/paste-markdown-as-org) :type 'user-error)))))

;;; james-functions-test.el ends here
