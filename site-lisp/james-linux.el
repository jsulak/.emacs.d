
;; Bind custom dired functions and fix search
(setq dired-load-hook
      (lambda (&rest ignore)
 (define-key dired-mode-map
   "l" 'dired-launch-command)
 (define-key dired-mode-map
   "f" 'dired-show-only)))
(put 'dired-find-alternate-file 'disabled nil)

(server-start)

(require 'james-gui)

(provide 'james-linux)
