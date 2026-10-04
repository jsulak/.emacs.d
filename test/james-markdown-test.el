;;; james-markdown-test.el --- Tests for Markdown helpers -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'markdown-mode)
(require 'james-markdown)

(ert-deftest james/zk-extract-uid-accepts-only-leading-digits ()
  (should (equal (james/zk--extract-uid "/notes/202607181200 Example.md")
                 "202607181200"))
  (should-not (james/zk--extract-uid "/notes/Example 202607181200.md")))

(ert-deftest james/zk-insert-link-inserts-selected-note-uid ()
  (james-test/with-temporary-directory directory
    (let ((james/zk-directory directory))
      (james-test/write-file (expand-file-name "202607181200 Example.md" directory)
                             "# Example\n")
      (with-temp-buffer
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "202607181200 Example.md")))
          (james/zk-insert-link))
        (should (equal (buffer-string) "[[202607181200]]"))))))

(ert-deftest james/zk-backlinks-builds-literal-wikilink-query ()
  (let ((james/zk-directory "/notes")
        received)
    (with-temp-buffer
      (setq buffer-file-name "/notes/202607181200 Example.md")
      (cl-letf (((symbol-function 'consult-ripgrep)
                 (lambda (&rest args) (setq received args))))
        (james/zk-backlinks)))
    (should (equal received '("/notes" "\\[\\[202607181200\\]\\]")))))

(ert-deftest james/zk-mode-enables-only-inside-configured-directory ()
  (james-test/with-temporary-directory directory
    (let ((james/zk-directory (expand-file-name "zettel" directory)))
      (james-test/write-file (expand-file-name "zettel/sub/note.md" directory) "# Note\n")
      (james-test/write-file (expand-file-name "zettel-other/note.md" directory) "# Note\n")
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "zettel/sub/note.md" directory))
        (markdown-mode)
        (should james/zk-mode))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "zettel-other/note.md" directory))
        (markdown-mode)
        (should-not james/zk-mode)))))

(ert-deftest james/zk-follow-id-link-opens-matching-note ()
  (james-test/with-temporary-directory directory
    (let* ((james/zk-directory directory)
           (source (expand-file-name "202607181200 Source.md" directory))
           (target (expand-file-name "202402101307 Target.md" directory)))
      (james-test/write-file source "See [[202402101307]].\n")
      (james-test/write-file target "# Target\n")
      (save-window-excursion
        (find-file source)
        (goto-char (point-min))
        (search-forward "202402101307")
        (backward-char 4)
        (should james/zk-mode)
        (markdown-follow-thing-at-point nil)
        (should (equal (buffer-file-name) target))
        (find-file source)
        (let ((link-position (progn
                               (goto-char (point-min))
                               (search-forward "202402101307")
                               (- (point) 4))))
          (cl-letf (((symbol-function 'mouse-set-point)
                     (lambda (_) (goto-char link-position))))
            (james/zk-follow-link-on-double-click 'click))
          (should (equal (buffer-file-name) target))))
      (should (eq (lookup-key james/zk-mode-map [double-mouse-1])
                  #'james/zk-follow-link-on-double-click))
      (should-not (lookup-key james/zk-mode-map [mouse-1]))
      (should-not (file-exists-p (expand-file-name "202402101307.md" directory))))))

(ert-deftest james/zk-follow-missing-id-does-not-create-note ()
  (james-test/with-temporary-directory directory
    (let ((james/zk-directory directory))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "202607181200 Source.md" directory))
        (markdown-mode)
        (insert "[[202402101307]]")
        (goto-char 5)
        (should-error (markdown-follow-thing-at-point nil) :type 'user-error)
        (should-not (file-exists-p
                     (expand-file-name "202402101307.md" directory)))))))

(ert-deftest james/markdown-indent-adds-heading-and-list-prefixes ()
  (with-temp-buffer
    (insert "## Heading\nBody\n- Item\n")
    (james/markdown-indent--jit-lock (point-min) (point-max))
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'line-prefix) ""))
    (forward-line 1)
    (should (equal (get-text-property (point) 'line-prefix) "    "))
    (forward-line 1)
    (should (equal (get-text-property (point) 'line-prefix) "    "))
    (should (equal (get-text-property (point) 'wrap-prefix) "      "))))

(ert-deftest james/markdown-indent-mode-cleans-prefix-properties ()
  (with-temp-buffer
    (markdown-mode)
    (insert "# Heading\nBody\n")
    (put-text-property (point-min) (point-max) 'line-prefix "  ")
    (put-text-property (point-min) (point-max) 'wrap-prefix "  ")
    (james/markdown-indent-mode -1)
    (should-not (text-property-not-all (point-min) (point-max)
                                       'line-prefix nil))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'wrap-prefix nil))))

;;; james-markdown-test.el ends here
