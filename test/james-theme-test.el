;;; james-theme-test.el --- Tests for custom themes -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'james-theme)

(ert-deftest james/apply-theme-switches-cleanly-between-annex-themes ()
  (let ((previous-themes custom-enabled-themes))
    (unwind-protect
        (progn
          (james/apply-theme 'annex-light)
          (should (equal custom-enabled-themes '(annex-light)))
          (should (equal (face-background 'default nil t) "#FAFAF8"))
          (james/apply-theme 'annex-dark)
          (should (equal custom-enabled-themes '(annex-dark)))
          (should (equal (face-background 'default nil t) "#1A1A1A")))
      (mapc #'disable-theme custom-enabled-themes)
      (dolist (theme (reverse previous-themes))
        (load-theme theme t)))))

;;; james-theme-test.el ends here
