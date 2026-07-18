;;; startup-smoke.el --- Full configuration startup smoke test -*- lexical-binding: t; -*-

(let* ((test-directory (file-name-directory (or load-file-name buffer-file-name)))
       (root-directory (file-name-directory (directory-file-name test-directory)))
       (state-directory (make-temp-file "james-emacs-smoke-state-" t)))
  (setq user-emacs-directory root-directory
        default-directory root-directory
        package-user-dir (expand-file-name "elpa" root-directory)
        james/load-custom-config nil
        james/load-local-config nil
        bookmark-default-file (expand-file-name "bookmarks" state-directory)
        recentf-save-file (expand-file-name "recentf" state-directory)
        savehist-file (expand-file-name "history" state-directory))
  (unwind-protect
      (progn
        (load (expand-file-name "early-init.el" root-directory) nil t)
        (load (expand-file-name "init.el" root-directory) nil t)
        (dolist (feature '(james-functions james-org rich2org))
          (unless (featurep feature)
            (error "Expected feature was not loaded: %s" feature)))
        (dolist (binding '(("C-h" . backward-delete-char-untabify)
                           ("<f1>" . help-command)
                           ("C-c p" . james/org-agenda-person-view)))
          (unless (eq (key-binding (kbd (car binding))) (cdr binding))
            (error "Unexpected binding for %s" (car binding))))
        (princ "Startup smoke test passed\n"))
    (delete-directory state-directory t)))

;;; startup-smoke.el ends here
