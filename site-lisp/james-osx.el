;;; james-osx.el --- macOS-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; macOS theme integration, modifier keys, and system file opening.

;;; Code:

(setq ring-bell-function 'ignore)

(if window-system
    (require 'james-gui)
  (require 'james-tty))

;; Sync theme with macOS dark/light appearance
(defun james/dark-mode-p ()
  "Return non-nil if macOS is in dark mode."
  (string-match-p
   "Dark"
   (shell-command-to-string "defaults read -g AppleInterfaceStyle 2>/dev/null")))

(defun james/apply-system-theme ()
  "Load annex-dark or annex-light to match macOS appearance."
  (let ((desired (if (james/dark-mode-p) 'annex-dark 'annex-light)))
    (unless (equal (car custom-enabled-themes) desired)
      (james/apply-theme desired))))

(james/apply-system-theme)
;; Re-check when Emacs regains focus. Works in GUI; also fires in terminals
;; that support focus reporting. In other terminals, run M-x
;; james/apply-system-theme manually after toggling System Settings.
(add-function :after after-focus-change-function #'james/apply-system-theme)

(defun james/open-file-mac (file-name)
  "Open FILE-NAME with its default macOS application."
  (if (file-exists-p file-name)
      (call-process "/usr/bin/open" nil 0 nil file-name)))

(declare-function dired-get-file-for-visit "dired")

(defun james/dired-open-mac ()
     "Open the Dired file at point with its default macOS application."
     (interactive)
     (let ((file-name (dired-get-file-for-visit)))
     (james/open-file-mac file-name)))


;; Defer tramp loading until it's actually needed
(with-eval-after-load 'tramp
  (setenv "TMPDIR" "/tmp"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(provide 'james-osx)

;;; james-osx.el ends here
