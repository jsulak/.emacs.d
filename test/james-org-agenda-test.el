;;; james-org-agenda-test.el --- Tests for Org workflows -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'org-agenda)
(require 'org-capture)

(ert-deftest james/org-open-loop-capture-templates-use-central-checklist ()
  (let ((undated (assoc "l" org-capture-templates))
        (dated (assoc "L" org-capture-templates)))
    (should (eq (nth 2 undated) 'plain))
    (should (equal (nth 3 undated)
                   '(file+function james/org-open-loops-file
                                   james/org-open-loops-goto-heading)))
    (should (equal (nth 4 undated)
                   "- [ ] %^{Who} - %^{What}\n"))
    (should (= (plist-get (nthcdr 5 undated) :empty-lines-after) 1))
    (should (eq (nth 2 dated) 'plain))
    (should (equal (nth 3 dated)
                   '(file+function james/org-open-loops-file
                                   james/org-open-loops-goto-heading)))
    (should (equal (nth 4 dated)
                   "- [ ] %^{Who} - %^{What} %^t\n"))
    (should (= (plist-get (nthcdr 5 dated) :empty-lines-after) 1))))

(ert-deftest james/org-open-loop-capture-leaves-space-before-next-heading ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory))
           (org-capture-templates
            '(("x" "Test open loop" plain
               (file+function james/org-open-loops-file
                              james/org-open-loops-goto-heading)
               "- [ ] %i\n"
               :immediate-finish t
               :empty-lines-after 1))))
      (james-test/write-file
       file
       "* TODO Open loops\n- [ ] Existing\n* TODO Next Heading\n")
      (org-capture-string "Alex Rivera - Review proposal" "x")
      (with-temp-buffer
        (insert-file-contents file)
        (should
         (equal (buffer-string)
                (concat
                 "* TODO Open loops\n"
                 "- [ ] Existing\n"
                 "- [ ] Alex Rivera - Review proposal\n\n"
                 "* TODO Next Heading\n")))))))

(ert-deftest james/org-task-to-open-loop-preserves-text-and-content ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory))
           (date (encode-time 0 0 12 18 11 2026)))
      (james-test/write-file
       file
       (concat "* TODO Tasks\n"
               "- [ ] Get material\n"
               "  Include the latest draft.\n"
               "  - [ ] Confirm the scope\n"
               "- [ ] Keep this task\n\n"
               "* TODO Open loops\n"
               "- [ ] Existing loop\n\n"
               "* Notes\nKeep this heading\n"))
      (james-test/write-file
       (expand-file-name "Alex Rivera.org" directory)
       "#+TITLE: Alex Rivera\n#+FILETAGS: :person:\n")
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (search-forward "latest draft")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "Alex"))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'org-read-date)
                   (lambda (&rest _) date)))
          (james/org-task-to-open-loop))
        (should (looking-at-p
                 "- \\[ \\] Alex Rivera - Get material <2026-11-18 Wed>"))
        (should
         (equal
          (buffer-string)
          (concat "* TODO Tasks\n"
                  "- [ ] Keep this task\n\n"
                  "* TODO Open loops\n"
                  "- [ ] Existing loop\n"
                  "- [ ] Alex Rivera - Get material <2026-11-18 Wed>\n"
                  "  Include the latest draft.\n"
                  "  - [ ] Confirm the scope\n\n"
                  "* Notes\nKeep this heading\n")))))))

(ert-deftest james/org-task-to-open-loop-allows-optional-fields ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory)))
      (james-test/write-file
       file "* TODO Tasks\n- [ ] Decide next step\n\n* TODO Open loops\n")
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (search-forward "Decide")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) ""))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) nil)))
          (james/org-task-to-open-loop))
        (should (equal (buffer-string)
                       "* TODO Tasks\n\n* TODO Open loops\n- [ ] Decide next step\n"))))))

(ert-deftest james/org-task-to-open-loop-rejects-ineligible-items ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory)))
      (james-test/write-file
       file
       (concat "* TODO Tasks\n"
               "- [X] Completed\n"
               "- [ ] Parent\n"
               "  - [ ] Nested\n\n"
               "* TODO Open loops\n"
               "- [ ] Existing\n"))
      (with-current-buffer (find-file-noselect file)
        (dolist (text '("Completed" "Nested" "Existing"))
          (goto-char (point-min))
          (search-forward text)
          (let ((before (buffer-string)))
            (should-error (james/org-task-to-open-loop) :type 'user-error)
            (should (equal before (buffer-string)))))))))

(ert-deftest james/org-task-to-open-loop-validates-before-prompting ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory)))
      (james-test/write-file file "* TODO Tasks\n- [ ] Task\n")
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (let ((before (buffer-string)))
          (should-error (james/org-task-to-open-loop) :type 'user-error)
          (should (equal before (buffer-string))))))))

(ert-deftest james/org-task-to-open-loop-rolls-back-edit-errors ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory)))
      (james-test/write-file
       file "* TODO Tasks\n- [ ] Task\n\n* TODO Open loops\n")
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (search-forward "Task\n")
        (let ((before (buffer-string)))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) ""))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) nil))
                    ((symbol-function 'james/org-open-loops-goto-heading)
                     (lambda () (error "Simulated insertion failure"))))
            (should-error (james/org-task-to-open-loop)))
          (should (equal before (buffer-string))))))))

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

(ert-deftest james/org-agenda-skip-excludes-central-open-loops-heading ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory))
           (james/org-agenda-person "Alex")
           (james/org-agenda-person-full "Alex Rivera"))
      (james-test/write-file
       file
       "* TODO Open Loops\n- [ ] Alex Rivera - Review proposal\n\n* TODO Ask Alex a question\n")
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (integerp (james/org-agenda-skip-unless-person)))
         (re-search-forward "^\\* TODO Ask Alex")
         (beginning-of-line)
         (should-not (james/org-agenda-skip-unless-person)))))))

(ert-deftest james/org-open-loops-items-selects-pending-person-items ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory))
           (james/org-agenda-person "alex")
           (james/org-agenda-person-full "Alex Rivera"))
      (james-test/write-file
       file
       (concat
        "* TODO Open loops\n"
        "- [ ] Alex Rivera - Review proposal <2026-09-01 Tue>\n"
        "- [X] Alex Rivera - Finished\n"
        "- [ ] Casey Jones - Other item\n\n"
        "* TODO Elsewhere\n"
        "- [ ] Alex Rivera - Not an open loop\n"))
      (let ((items (james/org-open-loops--items)))
        (should (equal (mapcar #'car items)
                       '("Alex Rivera - Review proposal <2026-09-01 Tue>")))
        (let ((marker (cdar items)))
          (should (markerp marker))
          (should (equal (buffer-file-name (marker-buffer marker)) file)))))))

(ert-deftest james/org-open-loops-agenda-block-handles-missing-file ()
  (james-test/with-temporary-directory directory
    (let ((org-directory directory)
          (james/org-agenda-person "Alex")
          (james/org-agenda-person-full "Alex Rivera"))
      (with-temp-buffer
        (james/org-open-loops-agenda-block)
        (should (equal (buffer-string)
                       "Open Loops\n  No pending open loops\n\n")))
      (should-not (file-exists-p
                   (expand-file-name "todo-tasks.org" directory))))))

(ert-deftest james/org-open-loops-agenda-block-links-to-source-items ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (file (expand-file-name "todo-tasks.org" directory))
           (james/org-agenda-person "Rivera")
           (james/org-agenda-person-full "Alex Rivera"))
      (james-test/write-file
       file
       "* TODO Open Loops\n- [ ] Alex Rivera - Review proposal\n")
      (with-temp-buffer
        (james/org-open-loops-agenda-block)
        (should (string-match-p
                 "  \\[ \\] Alex Rivera - Review proposal"
                 (buffer-string)))
        (goto-char (point-min))
        (re-search-forward "Review proposal")
        (let ((marker (get-text-property (point) 'org-marker)))
          (should (markerp marker))
          (should (equal (buffer-file-name (marker-buffer marker)) file)))))))

(ert-deftest james/org-person-agenda-command-prompts-and-renders-all-blocks ()
  (james-test/with-temporary-directory directory
    (let* ((org-directory directory)
           (org-agenda-files (list directory))
           (james/org-agenda-person "Stale Person")
           (james/org-agenda-person-full "Stale Person")
           (prompt-count 0))
      (james-test/write-file
       (expand-file-name "Alex Rivera.org" directory)
       "#+TITLE: Alex Rivera\n#+FILETAGS: :person:\n\n* Topics\n- [ ] Discuss roadmap\n")
      (james-test/write-file
       (expand-file-name "todo-tasks.org" directory)
       "* TODO Open Loops\n- [ ] Alex Rivera - Review proposal\n")
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (cl-incf prompt-count)
                       "Alex Rivera")))
            (org-agenda nil "p")
            (should (= prompt-count 1))
            (should (equal james/org-agenda-person "Alex Rivera"))
            (with-current-buffer "*Org Agenda*"
              (let ((agenda (buffer-string)))
                (should (string-match-p "1:1 Agenda Items" agenda))
                (should (string-match-p "Discuss roadmap" agenda))
                (should (string-match-p "Open Loops" agenda))
                (should (string-match-p "Review proposal" agenda))
                (should (string-match-p "Tasks" agenda)))))
        (when (get-buffer "*Org Agenda*")
          (kill-buffer "*Org Agenda*"))))))

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

(ert-deftest james/markdown-link-yank-adds-document-icons ()
  (with-temp-buffer
    (org-mode)
    (let ((kill-ring
           (list (concat
                  "[Slides](https://example.test/slides.pptx) "
                  "[Draft](draft.docx) [Brief](brief.pdf) "
                  "[Forecast](forecast.xlsx) "
                  "[Notes](notes.txt) [Photo](photo.png)"))))
      (yank))
    (should
     (equal
      (buffer-string)
      (concat
       "[[https://example.test/slides.pptx][📊 Slides]] "
       "[[draft.docx][📄 Draft]] [[brief.pdf][📕 Brief]] "
       "[[forecast.xlsx][📈 Forecast]] "
       "[[notes.txt][📎 Notes]] [[photo.png][Photo]]")))))

(ert-deftest james/markdown-link-yank-detects-document-from-description ()
  (with-temp-buffer
    (org-mode)
    (let ((kill-ring
           (list
            (concat
             "[Project Update - August 2026.pptx]("
             "https://tenant.sharepoint.test/:p:/s/ExampleTeam/"
             "opaque-document-id?e=example&"
             "CID=00000000-0000-0000-0000-000000000000&"
             "previoussessionid=11111111-1111-1111-1111-111111111111)"))))
      (yank))
    (should
     (equal
      (buffer-string)
      (concat
       "[[https://tenant.sharepoint.test/:p:/s/ExampleTeam/"
       "opaque-document-id?e=example&"
       "CID=00000000-0000-0000-0000-000000000000&"
       "previoussessionid=11111111-1111-1111-1111-111111111111]"
       "[📊 Project Update - August 2026.pptx]]")))))

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
