;;; run-tests.el --- Batch test runner -*- lexical-binding: t; -*-

(let* ((test-directory (file-name-directory (or load-file-name buffer-file-name)))
       (root-directory (file-name-directory (directory-file-name test-directory)))
       (state-directory (make-temp-file "james-emacs-test-state-" t)))
  (setq user-emacs-directory root-directory
        default-directory root-directory
        bookmark-default-file (expand-file-name "bookmarks" state-directory)
        recentf-save-file (expand-file-name "recentf" state-directory)
        savehist-file (expand-file-name "history" state-directory))
  (load (expand-file-name "early-init.el" root-directory) nil t)
  (load (expand-file-name "init.el" root-directory) nil t)
  (load (expand-file-name "james-org-test.el" test-directory) nil t))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
