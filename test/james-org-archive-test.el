;;; james-org-archive-test.el --- Checkbox archival tests -*- lexical-binding: t; -*-

(require 'test-helper)

(defmacro james-test/with-archive (source-text archive-text &rest body)
  "Create archive fixtures from SOURCE-TEXT and ARCHIVE-TEXT, then run BODY."
  (declare (indent 2))
  `(james-test/with-temporary-directory directory
     (let* ((org-directory directory)
            (org-mode-hook nil)
            (source-file (expand-file-name "todo-tasks.org" directory))
            (archive-file (expand-file-name "archive.org" directory)))
       (james-test/write-file source-file ,source-text)
       (james-test/write-file archive-file ,archive-text)
       ,@body)))

(defun james-test/archive-read (file)
  "Read FILE without visiting it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest james/org-archive-checkboxes-preserves-context-and-order ()
  (james-test/with-archive
      (concat "* TODO Tasks\n"
              "- [ ] Pending\n  - [X] Completed child stays\n"
              "- [X] Finished [[file:notes.org][notes]]\n"
              "  Continuation text\n\n  Another paragraph\n"
              "  - [X] Finished child\n"
              "- [X] Inconsistent parent\n  - [ ] Pending child\n"
              "- [X] Another inconsistent parent\n  - [-] Partial child\n"
              "- Plain parent\n  - [X] Nested completion stays\n\n"
              "* TODO Open loops\n- [X] Second completion\n\n"
              "* Notes\n#+begin_src text\n- [X] Example stays\n#+end_src\n")
      "#+TITLE: Tasks Archive\n* DONE Tasks Archive\n\n- [X] Old item\n\n* Other\nKeep\n"
    (should (= (james/org-archive-completed-checkboxes) 2))
    (let ((source (james-test/archive-read source-file))
          (archive (james-test/archive-read archive-file)))
      (dolist (text '("Pending" "Completed child stays" "Inconsistent parent"
                      "Another inconsistent parent" "Nested completion stays"
                      "Example stays" "* TODO Tasks" "* TODO Open loops"))
        (should (string-match-p (regexp-quote text) source)))
      (should-not (string-match-p "Second completion\\|Continuation text" source))
      (should (string-match-p
               (regexp-quote
                (concat "- [X] Old item\n\n"
                        "- [X] Finished [[file:notes.org][notes]]\n"
                        "  Continuation text\n\n  Another paragraph\n"
                        "  - [X] Finished child\n"
                        "- [X] Second completion\n\n* Other\nKeep\n"))
               archive))
      (should (= (james/org-archive-completed-checkboxes) 0))
      (should (equal source (james-test/archive-read source-file)))
      (should (equal archive (james-test/archive-read archive-file))))))

(ert-deftest james/org-archive-checkboxes-uses-live-buffers-and-widens ()
  (james-test/with-archive "* TODO Tasks\n- [X] Done\n\n* Notes\nKeep\n"
      "* DONE Tasks Archive\n"
    (with-current-buffer (find-file-noselect source-file)
      (goto-char (point-max))
      (insert "Unsaved note\n")
      (narrow-to-region (line-beginning-position -1) (point-max))
      (let ((position (point)))
        (should (= (james/org-archive-completed-checkboxes) 1))
        (should (buffer-narrowed-p))
        (should (< (point) position))
        (should (string-match-p "Unsaved note" (james-test/archive-read source-file)))
        (should-not (buffer-modified-p))))))

(ert-deftest james/org-archive-checkboxes-rejects-disk-conflicts ()
  (dolist (target '(source archive))
    (james-test/with-archive "* TODO Tasks\n- [X] Done\n"
        "* DONE Tasks Archive\n"
      (let* ((file (if (eq target 'source) source-file archive-file))
             (buffer (find-file-noselect file)))
        (james-test/write-file file "Externally changed contents\n")
        (with-current-buffer buffer
          (set-visited-file-modtime '(0 0 0 0)))
        (let ((source-before (james-test/archive-read source-file))
              (archive-before (james-test/archive-read archive-file)))
          (should-error (james/org-archive-completed-checkboxes) :type 'user-error)
          (should (equal source-before (james-test/archive-read source-file)))
          (should (equal archive-before (james-test/archive-read archive-file))))))))

(ert-deftest james/org-archive-checkboxes-archive-save-failure-keeps-source ()
  (james-test/with-archive "* TODO Tasks\n- [X] Done\n"
      "* DONE Tasks Archive\n"
    (let ((source-before (james-test/archive-read source-file))
          (archive-before (james-test/archive-read archive-file)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (error "Simulated write failure"))))
        (should-error (james/org-archive-completed-checkboxes)))
      (should (equal source-before (james-test/archive-read source-file)))
      (should (equal archive-before (james-test/archive-read archive-file)))
      (with-current-buffer (get-file-buffer archive-file)
        (should (equal archive-before (buffer-string)))))))

(ert-deftest james/org-archive-checkboxes-source-save-failure-is-recoverable ()
  (james-test/with-archive "* TODO Tasks\n- [X] Done\n"
      "* DONE Tasks Archive\n"
    (let ((save (symbol-function 'save-buffer)) saves)
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (push buffer-file-name saves)
                   (if (equal buffer-file-name source-file)
                       (error "Simulated source write failure")
                     (apply save args)))))
        (should-error (james/org-archive-completed-checkboxes) :type 'user-error))
      (should (equal (reverse saves) (list archive-file source-file))))
    (should (string-match-p "Done" (james-test/archive-read archive-file)))
    (with-current-buffer (get-file-buffer source-file)
      (should (buffer-modified-p))
      (should-not (string-match-p "Done" (buffer-string)))
      (save-buffer))
    (should-not (string-match-p "Done" (james-test/archive-read source-file)))
    (should (= (james/org-archive-completed-checkboxes) 0))))

(ert-deftest james/org-archive-checkboxes-requires-unambiguous-destination ()
  (dolist (archive '("* Notes\n"
                     "* DONE Tasks Archive\n* DONE Tasks Archive\n"))
    (james-test/with-archive "* TODO Tasks\n- [X] Done\n" archive
      (should-error (james/org-archive-completed-checkboxes) :type 'user-error)
      (should (string-match-p "Done" (james-test/archive-read source-file)))
      (should (equal archive (james-test/archive-read archive-file))))))

(provide 'james-org-archive-test)
;;; james-org-archive-test.el ends here
