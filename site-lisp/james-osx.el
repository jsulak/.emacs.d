;;; james-osx.el --- macOS-specific configuration -*- lexical-binding: t; -*-

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

;; Dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map "l" 'james/dired-open-mac)
  (define-key dired-mode-map "f" 'james/dired-show-only))


(defun james/open-file-mac (file-name)
  (if (file-exists-p file-name)
      (call-process "/usr/bin/open" nil 0 nil file-name)))

(declare-function dired-get-file-for-visit "dired")

(defun james/dired-open-mac ()
     (interactive)
     (let ((file-name (dired-get-file-for-visit)))
     (james/open-file-mac file-name)))


;; Defer tramp loading until it's actually needed
(with-eval-after-load 'tramp
  (setenv "TMPDIR" "/tmp"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)



