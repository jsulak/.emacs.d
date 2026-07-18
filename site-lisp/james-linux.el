;;; james-linux.el --- Linux-specific configuration -*- lexical-binding: t; -*-

(if window-system
    (require 'james-gui)
  (require 'james-tty))

(provide 'james-linux)
