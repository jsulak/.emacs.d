;;; james-theme.el --- Theme helpers shared across GUI and TTY -*- lexical-binding: t; -*-

;; Register custom theme directory
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(defun james/apply-theme (theme)
  "Disable all active themes and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'james-theme)
