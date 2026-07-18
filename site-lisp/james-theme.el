;;; james-theme.el --- Theme helpers shared across GUI and TTY -*- lexical-binding: t; -*-

;;; Commentary:
;; Locate and switch between the custom Annex themes.

;;; Code:

;; Register custom theme directory
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes"
                               (or (bound-and-true-p james/config-directory)
                                   user-emacs-directory)))

(defun james/apply-theme (theme)
  "Disable all active themes and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'james-theme)

;;; james-theme.el ends here
