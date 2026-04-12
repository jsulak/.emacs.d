;;; james-tty.el --- Terminal-only settings -*- lexical-binding: t; -*-

;; Loaded only when Emacs is running in a terminal (no window system).
;;
;; The annex themes use 24-bit hex colors. For them to render correctly the
;; terminal must advertise truecolor support. Modern Emacs (29+) enables this
;; automatically when TERM ends in `-direct' (e.g. `xterm-direct',
;; `tmux-direct', `alacritty-direct'). If colors look wrong, start Emacs with
;; `TERM=xterm-direct emacs -nw' or configure your terminal/terminfo.

(require 'james-theme)

(menu-bar-mode -1)

;; Mouse support in terminals that report mouse events
(xterm-mouse-mode 1)

;; Default to light; james-osx.el overrides with system-aware switching.
(james/apply-theme 'annex-light)

(provide 'james-tty)
