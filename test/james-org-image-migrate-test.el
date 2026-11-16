;;; james-org-image-migrate-test.el --- Org image migration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'james-org-image-migrate)

(defmacro james-test/with-image-migration (&rest body)
  "Run BODY with a temporary Org collection and fake OCR engine."
  (declare (indent 0) (debug t))
  `(james-test/with-temporary-directory root
     (let* ((manifest (expand-file-name "migration.tsv" root))
            (james/org-image-migrate-require-backup nil)
            (james/org-image-migrate-ocr-program
             (expand-file-name "fake-tesseract" root)))
       (james-test/write-file
        james/org-image-migrate-ocr-program
        (concat
         "#!/bin/sh\n"
         "for arg in \"$@\"; do\n"
         "  if [ \"$arg\" = tsv ]; then\n"
         "    printf '%s\\n' 'level\tpage_num\tblock_num\tpar_num\tline_num\tword_num\tleft\ttop\twidth\theight\tconf\ttext' '1\t1\t0\t0\t0\t0\t0\t0\t1600\t900\t-1\t' '5\t1\t1\t1\t1\t1\t100\t50\t400\t70\t96\tGenerated' '5\t1\t1\t1\t1\t2\t520\t50\t250\t70\t96\tSlide' '5\t1\t1\t1\t1\t3\t790\t50\t250\t70\t96\tTitle'\n"
         "    exit 0\n"
         "  fi\n"
         "done\n"
         "printf '%s\\n' 'Generated Slide Title' 'Body text'\n"))
       (set-file-modes james/org-image-migrate-ocr-program #o700)
       ,@body)))

(defun james-test/image-migration-fixture (root)
  "Create a representative image migration fixture beneath ROOT."
  (let* ((image-directory (expand-file-name "images/notes" root))
         (compact (expand-file-name "202601011200-clipboard.png"
                                    image-directory))
         (legacy (expand-file-name "20260102-130405-clipboard.png"
                                   image-directory))
         (orphan (expand-file-name "202601031400-orphan.png"
                                   image-directory))
         (org-file (expand-file-name "notes.org" root)))
    (james-test/write-file compact "compact image")
    (james-test/write-file legacy "legacy image")
    (james-test/write-file orphan "orphan image")
    (james-test/write-file
     org-file
     (format (concat "* Slides\n"
                     "[[file:images/notes/%s][Existing slide]]\n"
                     ":OCR:\n"
                     ": sha256: %s\n"
                     ": options: (\"-l\" \"eng\" \"--psm\" \"11\")\n"
                     ": Existing Slide Title\n"
                     ": Existing body\n"
                     ":END:\n\n"
                     "[[file:images/notes/%s]]\n")
             (file-name-nondirectory compact)
             (james/org-image-migrate--hash-file compact)
             (file-name-nondirectory legacy)))
    (list org-file compact legacy orphan)))

(ert-deftest james/org-image-migrate-plans-ocr-timestamps-and-orphans ()
  (james-test/with-image-migration
    (pcase-let* ((`(,_org-file ,_compact ,_legacy ,orphan)
                  (james-test/image-migration-fixture root))
                 (rows (james/org-image-migrate-plan root manifest))
                 (existing (nth 0 rows))
                 (generated (nth 1 rows)))
      (should (= (length rows) 2))
      (should (equal (james/org-image-migrate-row-ocr-source existing)
                     "drawer"))
      (should (equal (james/org-image-migrate-row-new-path existing)
                     "images/notes/202601011200-generated-slide-title.png"))
      (should (equal (james/org-image-migrate-row-ocr-source generated)
                     "generated"))
      (should (equal (james/org-image-migrate-row-new-path generated)
                     "images/notes/202601021304-generated-slide-title.png"))
      (should (file-regular-p
               (expand-file-name
                (james/org-image-migrate-row-cache generated)
                (concat manifest ".ocr/"))))
      (with-temp-buffer
        (insert-file-contents (concat manifest ".orphans.tsv"))
        (should (string-match-p
                 (regexp-quote (file-relative-name orphan root))
                 (buffer-string)))))))

(ert-deftest james/org-image-migrate-applies-and-verifies-reviewed-plan ()
  (james-test/with-image-migration
    (pcase-let* ((`(,org-file ,compact ,legacy ,orphan)
                  (james-test/image-migration-fixture root))
                 (_rows (james/org-image-migrate-plan root manifest))
                 (compact-target
                  (expand-file-name
                   "images/notes/202601011200-generated-slide-title.png" root))
                 (legacy-target
                  (expand-file-name
                   "images/notes/202601021304-generated-slide-title.png" root)))
      (james/org-image-migrate-apply root manifest)
      (should-not (file-exists-p compact))
      (should-not (file-exists-p legacy))
      (should (file-regular-p compact-target))
      (should (file-regular-p legacy-target))
      (should (file-regular-p orphan))
      (with-temp-buffer
        (insert-file-contents org-file)
        (should (string-match-p "202601011200-generated-slide-title.png"
                                (buffer-string)))
        (should (string-match-p "\[Existing slide\]" (buffer-string)))
        (should (string-match-p "202601021304-generated-slide-title.png"
                                (buffer-string)))
        (should (= 2 (how-many "^:OCR:$" (point-min) (point-max)))))
      (should (james/org-image-migrate-verify root manifest)))))

(ert-deftest james/org-image-migrate-blocks-review-and-stale-images ()
  (james-test/with-image-migration
    (let ((image (expand-file-name "images/note/202601011200-slide.png" root))
          (org-file (expand-file-name "note.org" root)))
      (james-test/write-file image "image")
      (james-test/write-file org-file
                             "[[file:images/note/202601011200-slide.png]]\n")
      (james-test/write-file
       james/org-image-migrate-ocr-program
       (concat
        "#!/bin/sh\n"
        "for arg in \"$@\"; do\n"
        "  if [ \"$arg\" = tsv ]; then\n"
        "    printf '%s\\n' 'level\tpage_num\tblock_num\tpar_num\tline_num\tword_num\tleft\ttop\twidth\theight\tconf\ttext' '1\t1\t0\t0\t0\t0\t0\t0\t1600\t900\t-1\t'\n"
        "    exit 0\n"
        "  fi\n"
        "done\n"
        "printf '%s\\n' '|'\n"))
      (james/org-image-migrate-plan root manifest)
      (pcase-let ((`(,_metadata . ,rows)
                   (james/org-image-migrate--read-manifest manifest)))
        (should (equal (james/org-image-migrate-row-warning (car rows))
                       "no-layout-title")))
      (should-error (james/org-image-migrate-apply root manifest))
      (with-temp-buffer
        (insert-file-contents manifest)
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "review\t")
            (replace-match "skip\t"))
          (forward-line 1))
        (write-region nil nil manifest nil 'silent))
      (james-test/write-file image "changed image")
      (should-error (james/org-image-migrate-apply root manifest)))))

(ert-deftest james/org-image-migrate-rolls-back-files-on-write-failure ()
  (james-test/with-image-migration
    (pcase-let* ((`(,_org-file ,compact ,legacy ,_orphan)
                  (james-test/image-migration-fixture root))
                 (_rows (james/org-image-migrate-plan root manifest)))
      (cl-letf (((symbol-function 'james/org-image-migrate--atomic-write)
                 (lambda (&rest _) (error "Simulated write failure"))))
        (should-error (james/org-image-migrate-apply root manifest)))
      (should (file-regular-p compact))
      (should (file-regular-p legacy))
      (with-temp-buffer
        (insert-file-contents (concat manifest ".journal.tsv"))
        (should (string-match-p "state\trolled-back" (buffer-string)))))))

(ert-deftest james/org-image-migrate-batch-accepts-command-separator ()
  (let ((command-line-args-left '("--" "plan" "manifest.tsv"))
        (process-environment (copy-sequence process-environment))
        received)
    (setenv "ORG_SOURCE_DIR" "/tmp/example-org")
    (cl-letf (((symbol-function 'james/org-image-migrate-plan)
               (lambda (root manifest) (setq received (list root manifest)))))
      (james/org-image-migrate-batch))
    (should (equal received '("/tmp/example-org" "manifest.tsv")))))

(ert-deftest james/org-image-migrate-reads-crlf-manifests ()
  (james-test/with-image-migration
    (james-test/image-migration-fixture root)
    (james/org-image-migrate-plan root manifest)
    (with-temp-buffer
      (insert-file-contents manifest)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\r\n" t t))
      (write-region nil nil manifest nil 'silent))
    (pcase-let ((`(,_metadata . ,rows)
                 (james/org-image-migrate--read-manifest manifest)))
      (should (= (length rows) 2))
      (should-not
       (string-suffix-p "\r" (james/org-image-migrate-row-cache (cadr rows)))))))

(ert-deftest james/org-image-migrate-decodes-spreadsheet-tsv-fields ()
  (should (equal
           (james/org-image-migrate--decode-field
            "\"images/Reviews (SIOP, Dev Review)/slide.png\"")
           "images/Reviews (SIOP, Dev Review)/slide.png"))
  (should (equal (james/org-image-migrate--decode-field "\"Say \"\"Hi\"\"\"")
                 "Say \"Hi\"")))

(ert-deftest james/org-image-migrate-apply-rejects-open-org-buffers ()
  (james-test/with-image-migration
    (pcase-let* ((`(,org-file ,_compact ,_legacy ,_orphan)
                  (james-test/image-migration-fixture root))
                 (_rows (james/org-image-migrate-plan root manifest))
                 (lock (expand-file-name
                        (concat ".#" (file-name-nondirectory org-file))
                        (file-name-directory org-file))))
      (james-test/write-file lock "locked")
      (unwind-protect
          (should-error (james/org-image-migrate-apply root manifest))
        (delete-file lock)))))

(ert-deftest james/org-image-title-layout-prefers-substantial-title-line ()
  (let* ((tsv
          (mapconcat
           #'identity
           '("level\tpage_num\tblock_num\tpar_num\tline_num\tword_num\tleft\ttop\twidth\theight\tconf\ttext"
             "1\t1\t0\t0\t0\t0\t0\t0\t2560\t1440\t-1\t"
             "5\t1\t1\t1\t1\t1\t2100\t64\t300\t123\t95\tUpdated"
             "5\t1\t2\t1\t1\t1\t100\t60\t150\t74\t94\tR3"
             "5\t1\t2\t1\t1\t2\t270\t60\t200\t74\t95\tSW"
             "5\t1\t2\t1\t1\t3\t490\t60\t400\t74\t96\tProductivity"
             "5\t1\t2\t1\t1\t4\t910\t60\t280\t74\t96\tGrowth"
             "5\t1\t2\t1\t1\t5\t1210\t60\t100\t74\t96\tto"
             "5\t1\t2\t1\t1\t6\t1330\t60\t130\t74\t96\tQ3"
             "5\t1\t2\t1\t1\t7\t1480\t60\t260\t74\t96\t2026"
             "5\t1\t2\t1\t1\t8\t1760\t60\t220\t74\t96\tEAC"
             "5\t1\t3\t1\t1\t1\t100\t900\t900\t100\t98\tBody")
           "\n"))
         (result (james/org-image-title-from-tsv tsv)))
    (should (equal (plist-get result :title)
                   "R3 SW Productivity Growth to Q3 2026 EAC"))))

(ert-deftest james/org-image-title-layout-requires-three-characters ()
  (let ((header
         "level\tpage_num\tblock_num\tpar_num\tline_num\tword_num\tleft\ttop\twidth\theight\tconf\ttext")
        (page "1\t1\t0\t0\t0\t0\t0\t0\t1000\t600\t-1\t"))
    (should-not
     (james/org-image-title-from-tsv
      (string-join
       (list header page "5\t1\t1\t1\t1\t1\t50\t30\t200\t60\t95\tAI")
       "\n")))
    (should
     (equal
      (plist-get
       (james/org-image-title-from-tsv
        (string-join
         (list header page "5\t1\t1\t1\t1\t1\t50\t30\t200\t60\t95\tCAP")
         "\n"))
       :title)
      "CAP"))))

;;; james-org-image-migrate-test.el ends here
