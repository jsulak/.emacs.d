;;; james-linux.el --- Linux-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Select GUI or terminal configuration on Linux.

;;; Code:

(if window-system
    (require 'james-gui)
  (require 'james-tty))

(provide 'james-linux)

;;; james-linux.el ends here
