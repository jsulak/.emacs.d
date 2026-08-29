;;; static-checks.el --- Built-in static checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run syntax, documentation, and declaration checks without external packages.

;;; Code:

(require 'check-declare)
(require 'checkdoc)
(require 'cl-lib)

(let* ((test-directory (file-name-directory (or load-file-name buffer-file-name)))
       (root-directory (file-name-directory (directory-file-name test-directory)))
       (source-files
        (append
         (directory-files (expand-file-name "site-lisp" root-directory)
                          t "\\.el\\'")
         (directory-files (expand-file-name "themes" root-directory)
                          t "\\.el\\'")))
       (all-files
        (append
         (list (expand-file-name "early-init.el" root-directory)
               (expand-file-name "init.el" root-directory))
         source-files
         (directory-files test-directory t "\\.el\\'")))
       failures)
  (dolist (file all-files)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (condition-case error-data
          (check-parens)
        (error
         (push (format "%s: %s"
                       (file-relative-name file root-directory)
                       (error-message-string error-data))
               failures)))))
  (cl-letf (((symbol-function 'display-warning)
             (lambda (_type message &rest _)
               (push (string-trim message) failures))))
    (dolist (file source-files)
      (checkdoc-file file)))
  (dolist (file source-files)
    (when-let* ((errors (check-declare-file file)))
      (push (format "%s: declaration errors: %S"
                    (file-relative-name file root-directory) errors)
            failures)))
  (if failures
      (progn
        (dolist (failure (nreverse failures))
          (princ (format "%s\n" failure)))
        (kill-emacs 1))
    (princ "Static checks passed\n")))

;;; static-checks.el ends here
