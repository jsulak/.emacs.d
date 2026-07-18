;;; test-helper.el --- Shared test setup -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'package)

(defconst james-test/root-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the configuration under test.")

(defconst james-test/state-directory
  (make-temp-file "james-emacs-test-state-" t)
  "Temporary directory for state created by tests.")

(setq user-emacs-directory james-test/root-directory
      default-directory james-test/root-directory
      package-user-dir (expand-file-name "elpa" james-test/root-directory)
      custom-file (expand-file-name "custom.el" james-test/state-directory)
      bookmark-default-file (expand-file-name "bookmarks" james-test/state-directory)
      recentf-save-file (expand-file-name "recentf" james-test/state-directory)
      savehist-file (expand-file-name "history" james-test/state-directory))

(add-to-list 'load-path (expand-file-name "site-lisp" james-test/root-directory))
(package-initialize)
(require 'use-package)

;; Unit tests must never install packages or refresh package archives.  Explicit
;; package requirements below provide a clear failure when a test dependency is
;; missing.
(setq use-package-always-ensure nil
      use-package-ensure-function #'ignore)

(require 'org-download)
(require 'james-functions)
(require 'james-org)

(defun james-test/write-file (filename contents)
  "Write CONTENTS to FILENAME, creating its parent directory."
  (make-directory (file-name-directory filename) t)
  (with-temp-file filename (insert contents)))

(defmacro james-test/with-temporary-directory (binding &rest body)
  "Bind BINDING to a temporary directory while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,binding (make-temp-file "james-emacs-test-" t)))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let ((file (buffer-local-value 'buffer-file-name buffer)))
           (when (file-in-directory-p file ,binding)
             (kill-buffer buffer))))
       (delete-directory ,binding t))))

(provide 'test-helper)
;;; test-helper.el ends here
