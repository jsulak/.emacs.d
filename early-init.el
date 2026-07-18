;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;; Never let stale local bytecode shadow newer configuration source.
(setq load-prefer-newer t)

;; Disable UI chrome before frame is drawn (avoids flicker)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
