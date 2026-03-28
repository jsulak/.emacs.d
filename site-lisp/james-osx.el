(menu-bar-mode 1)

(setq ring-bell-function 'ignore)

(if window-system
    (require 'james-gui))

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))


;; Dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map "l" 'james/dired-open-mac)
  (define-key dired-mode-map "f" 'james/dired-show-only))


(defun james/open-file-mac (file-name)
  (if (file-exists-p file-name)
      (call-process "/usr/bin/open" nil 0 nil file-name)))

(defun james/dired-open-mac ()
     (interactive)
     (let ((file-name (dired-get-file-for-visit)))
     (james/open-file-mac file-name)))


;; Defer tramp loading until it's actually needed
(with-eval-after-load 'tramp
  (setenv "TMPDIR" "/tmp"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

