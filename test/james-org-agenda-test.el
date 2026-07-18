;;; james-org-agenda-test.el --- Tests for Org workflows -*- lexical-binding: t; -*-

(require 'test-helper)

(ert-deftest james/org-1on1-people-discovers-tagged-files ()
  (james-test/with-temporary-directory directory
    (let ((org-directory directory))
      (james-test/write-file
       (expand-file-name "alex.org" directory)
       "#+TITLE: Alex Rivera\n#+FILETAGS: :person:\n")
      (james-test/write-file
       (expand-file-name "casey-jones.org" directory)
       "#+FILETAGS: :person:team:\n")
      (james-test/write-file
       (expand-file-name "project.org" directory)
       "#+TITLE: Project\n#+FILETAGS: :work:\n")
      (should (equal (james/org-1on1-people)
                     '("Alex Rivera" "casey-jones"))))))

(ert-deftest james/org-1on1-resolve-person-supports-common-match-styles ()
  (let ((people '("Alex Rivera" "Casey Jones")))
    (should (equal (james/org-1on1-resolve-person "Alex Rivera" people)
                   "Alex Rivera"))
    (should (equal (james/org-1on1-resolve-person "case" people)
                   "Casey Jones"))
    (should (equal (james/org-1on1-resolve-person "River" people)
                   "Alex Rivera"))
    (should (equal (james/org-1on1-resolve-person "New Person" people)
                   "New Person"))))

(ert-deftest james/org-1on1-person-file-creates-expected-structure ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (james/org-1on1-person-file "Alex Rivera")))
      (should (file-exists-p file))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     "#+TITLE: Alex Rivera\n#+FILETAGS: :person:\n\n* Topics\n\n")))))

(ert-deftest james/org-1on1-add-item-appends-under-topics ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "Alex Rivera.org" directory)))
      (james-test/write-file
       file
       "#+TITLE: Alex Rivera\n#+FILETAGS: :person:\n\n* Topics\n\n- [x] Done\n\n* Notes\nKeep me\n")
      (should (equal (james/org-1on1-add-item "Alex Rivera" "Review proposal")
                     "Added to Alex Rivera: Review proposal"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((item-position
               (progn
                 (should (re-search-forward "^- \\[ \\] Review proposal$" nil t))
                 (match-beginning 0)))
              (notes-position
               (progn
                 (should (re-search-forward "^\\* Notes\nKeep me$" nil t))
                 (match-beginning 0))))
          (should (< item-position notes-position)))))))

(ert-deftest james/org-1on1-goto-topics-creates-missing-heading ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Alex\n")
    (james/org-1on1-goto-topics)
    (insert "- [ ] New item\n")
    (should (equal (buffer-string)
                   "#+TITLE: Alex\n* Topics\n\n- [ ] New item\n"))))

(ert-deftest james/org-agenda-skip-unless-person-matches-heading-or-body ()
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Follow up\nAsk Alex about the proposal\n* TODO Other\nNo match\n")
    (let ((james/org-agenda-person "Alex Rivera")
          (james/org-agenda-person-full "Alex Rivera"))
      (goto-char (point-min))
      (should-not (james/org-agenda-skip-unless-person))
      (re-search-forward "^\\* TODO Other")
      (beginning-of-line)
      (should (integerp (james/org-agenda-skip-unless-person))))))

(ert-deftest james/org-1on1-agenda-block-lists-only-pending-items ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (person "Alex Rivera")
           (file (expand-file-name (concat person ".org") directory))
           (james/org-agenda-person person)
           (james/org-agenda-person-full person))
      (james-test/write-file
       file
       "#+TITLE: Alex Rivera\n#+FILETAGS: :person:\n\n* Topics\n- [ ] Pending\n- [X] Finished\n")
      (with-temp-buffer
        (james/org-1on1-agenda-block)
        (should (string-match-p "  \\[ \\] Pending" (buffer-string)))
        (should-not (string-match-p "Finished" (buffer-string)))
        (goto-char (point-min))
        (re-search-forward "Pending")
        (let ((marker (get-text-property (point) 'org-marker)))
          (should (markerp marker))
          (should (equal (buffer-file-name (marker-buffer marker)) file)))))))

(ert-deftest james/markdown-link-yank-converts-only-in-org-mode ()
  (with-temp-buffer
    (org-mode)
    (let ((kill-ring '("See [Example](https://example.test).")))
      (yank))
    (should (equal (buffer-string)
                   "See [[https://example.test][Example]].")))
  (with-temp-buffer
    (text-mode)
    (let ((kill-ring '("[Example](https://example.test)")))
      (yank))
    (should (equal (buffer-string)
                   "[Example](https://example.test)"))))

(ert-deftest james/org-insert-file-link-uses-title-and-fallback ()
  (james-test/with-temporary-directory directory
    (let ((org-directory directory))
      (james-test/write-file (expand-file-name "titled.org" directory)
                             "#+TITLE: A Useful Title\n")
      (james-test/write-file (expand-file-name "plain.org" directory)
                             "Body\n")
      (with-temp-buffer
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "titled.org")))
          (james/org-insert-file-link))
        (should (equal (buffer-string)
                       "[[file:titled.org][A Useful Title]]")))
      (with-temp-buffer
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "plain.org")))
          (james/org-insert-file-link))
        (should (equal (buffer-string)
                       "[[file:plain.org][plain]]"))))))

;;; james-org-agenda-test.el ends here
