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
