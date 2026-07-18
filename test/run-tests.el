;;; run-tests.el --- Hermetic batch test runner -*- lexical-binding: t; -*-

(let ((test-directory (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helper.el" test-directory) nil t)
  (dolist (test-file (directory-files test-directory t "-test\\.el\\'"))
    (load test-file nil t)))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
