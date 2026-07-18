;;; byte-compile.el --- Compile configuration with warnings as errors -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch entry point for strict byte compilation without writing .elc files into
;; the repository.

;;; Code:

(require 'bytecomp)
(require 'package)

(let* ((test-directory (file-name-directory (or load-file-name buffer-file-name)))
       (root-directory (file-name-directory (directory-file-name test-directory)))
       (output-directory (make-temp-file "james-emacs-byte-compile-" t))
       (user-emacs-directory root-directory)
       (package-user-dir (expand-file-name "elpa" root-directory))
       (default-directory root-directory)
       (byte-compile-error-on-warn t)
       (byte-compile-warnings t)
       (byte-compile-dest-file-function
        (lambda (source)
          (let ((destination
                 (expand-file-name
                  (concat (file-relative-name source root-directory) "c")
                  output-directory)))
            (make-directory (file-name-directory destination) t)
            destination))))
  (unwind-protect
      (progn
        (add-to-list 'load-path (expand-file-name "site-lisp" root-directory))
        (package-initialize)
        (require 'use-package)
        (load (expand-file-name "test-helper.el" test-directory) nil t)
        (let ((files
               (append
                (list (expand-file-name "early-init.el" root-directory)
                      (expand-file-name "init.el" root-directory))
                (directory-files (expand-file-name "site-lisp" root-directory)
                                 t "\\.el\\'")
                (directory-files (expand-file-name "themes" root-directory)
                                 t "\\.el\\'")
                (directory-files test-directory t "-test\\.el\\'")
                (list (expand-file-name "test-helper.el" test-directory)))))
          (dolist (file files)
            (princ (format "Byte-compiling %s\n"
                           (file-relative-name file root-directory)))
            (unless (byte-compile-file file)
              (error "Byte compilation failed for %s" file))))
        (princ "Strict byte compilation passed\n"))
    (delete-directory output-directory t)))

;;; byte-compile.el ends here
