;;; james-org-image.el --- Shared Org image naming helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Filesystem-safe naming helpers shared by interactive image insertion and
;; batch image migrations.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defconst james/org-image-extensions
  '("avif" "bmp" "gif" "heic" "jpeg" "jpg" "png" "svg" "tif" "tiff"
    "webp")
  "Filename extensions routed through the Org image workflow.")

(defun james/org-image--sanitized-name (filename)
  "Return a lowercase, filesystem-friendly image name for FILENAME."
  (let* ((filename (file-name-nondirectory filename))
         (extension (file-name-extension filename))
         (stem (downcase (file-name-base filename))))
    (setq stem (replace-regexp-in-string "[^[:alnum:]]+" "-" stem)
          stem (string-trim stem "-+" "-+"))
    (when extension
      (setq extension
            (replace-regexp-in-string "[^[:alnum:]]" "" (downcase extension))))
    (concat (if (string-empty-p stem) "image" stem)
            (if (and extension (not (string-empty-p extension)))
                (concat "." extension)
              ""))))

(defun james/org-image--unique-name (directory filename)
  "Return an available FILENAME in DIRECTORY, adding a numeric suffix."
  (if (not (file-exists-p (expand-file-name filename directory)))
      filename
    (let* ((extension (file-name-extension filename t))
           (stem (file-name-sans-extension filename))
           (suffix 2)
           candidate)
      (while
          (progn
            (setq candidate (format "%s-%d%s" stem suffix (or extension ""))
                  suffix (1+ suffix))
            (file-exists-p (expand-file-name candidate directory))))
      candidate)))

(defun james/org--timestamp-prefix ()
  "Return the compact timestamp prefix used for Org file names."
  (format-time-string "%Y%m%d%H%M-"))

(defun james/org-image-file-p (filename)
  "Return non-nil when FILENAME has a supported image extension."
  (member (downcase (or (file-name-extension filename) ""))
          james/org-image-extensions))

(defun james/org-image--tsv-number (fields index)
  "Return numeric value at INDEX in TSV FIELDS."
  (string-to-number (or (nth index fields) "0")))

(defun james/org-image-title-from-tsv (tsv)
  "Return layout metadata for the most likely slide title in Tesseract TSV.
The result is a plist containing `:title', `:confidence', `:top', `:height',
and `:score'.  Return nil when no sufficiently strong upper-page line exists."
  (let* ((rows (cdr (split-string tsv "\n" t)))
         (groups (make-hash-table :test #'equal))
         page-width page-height words)
    (dolist (row rows)
      (let ((fields (split-string row "\t" nil)))
        (when (>= (length fields) 12)
          (pcase (james/org-image--tsv-number fields 0)
            (1
             (setq page-width (james/org-image--tsv-number fields 8)
                   page-height (james/org-image--tsv-number fields 9)))
            (5
             (unless (string-empty-p (string-trim (nth 11 fields)))
               (let ((key (mapconcat
                           (lambda (index) (or (nth index fields) ""))
                           '(1 2 3 4) ":")))
                 (puthash key (nconc (gethash key groups) (list fields)) groups)
                 (push fields words))))))))
    (unless (and page-width (> page-width 0) page-height (> page-height 0))
      (setq page-width
            (cl-loop for fields in words
                     maximize (+ (james/org-image--tsv-number fields 6)
                                 (james/org-image--tsv-number fields 8)))
            page-height
            (cl-loop for fields in words
                     maximize (+ (james/org-image--tsv-number fields 7)
                                 (james/org-image--tsv-number fields 9)))))
    (when (and page-width page-height (> page-width 0) (> page-height 0))
      (let (candidates)
        (maphash
         (lambda (_key fields)
           (let* ((text (string-join (mapcar (lambda (row) (nth 11 row)) fields)
                                     " "))
                  (characters (length (replace-regexp-in-string
                                       "[^[:alnum:]]" "" text)))
                  (left (cl-loop for row in fields
                                 minimize (james/org-image--tsv-number row 6)))
                  (right (cl-loop for row in fields
                                  maximize (+ (james/org-image--tsv-number row 6)
                                              (james/org-image--tsv-number row 8))))
                  (top (cl-loop for row in fields
                                minimize (james/org-image--tsv-number row 7)))
                  (height (cl-loop for row in fields
                                   maximize (james/org-image--tsv-number row 9)))
                  (confidences
                   (cl-loop for row in fields
                            for confidence = (james/org-image--tsv-number row 10)
                            when (>= confidence 0) collect confidence))
                  (confidence (if confidences
                                  (/ (apply #'+ confidences)
                                     (float (length confidences)))
                                0.0))
                  (width (- right left)))
             (when (and (>= characters 3)
                        (< top (* page-height 0.35))
                        (>= confidence 40.0)
                        (>= width (* page-width 0.05)))
               (push
                (list :title text :confidence confidence :top top :height height
                      :score (* height (log (1+ characters))
                                (- 1.0 (* 0.35 (/ top (float page-height))))))
                candidates))))
         groups)
        (car (sort candidates
                   (lambda (a b) (> (plist-get a :score)
                                    (plist-get b :score)))))))))

(provide 'james-org-image)
;;; james-org-image.el ends here
