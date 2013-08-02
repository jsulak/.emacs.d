
;; http://whattheemacsd.com//editing-defuns.el-01.html
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Tag navigation
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "C-.") 'pop-tag-mark)

;; Expand region
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-\"") 'er/contract-region)

;; Eshell
;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c j") 'dired-jump)
(global-set-key (kbd "C-c l") 'open-current-buffer-mac)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-c E") (lambda () (interactive) (eshell t)))


(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key (kbd "C-c z") 'zap-up-to-char)

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "M-;") 'hippie-expand)
(global-set-key (kbd "C-=") 'hippie-expand)

;; BM mode
(global-set-key (kbd "<M-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)


;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)
;; duplicate a line and comment the first
(global-set-key (kbd "C-c c") (lambda()(interactive)(djcb-duplicate-line t)))
(global-set-key "\C-x\C-j" 'join-line)

;; Redefine comment-dwim to comment out whole line
(global-set-key (kbd "C-;") 'comment-dwim-line)
(global-set-key (kbd "C-c ;") 'comment-dwim-line)

;;Set ctrl-z to undo
(global-set-key "\C-z" 'undo)

;;set up kill word keyboard bindings
(global-set-key "\C-w" 'kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-q" 'backward-kill-word)
(global-set-key "\C-c\C-q" 'quoted-insert)

;; delete enclosed text
(global-set-key (kbd "C-c d") 'delete-enclosed-text)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; Run shell command
(global-set-key (kbd "\C-c s") 'shell-command)

;; Set goto-line
(global-set-key "\C-x\C-g" 'goto-line)

;;bind repeat last macro to F5
(global-set-key [(f4)] 'call-last-kbd-macro)
(global-set-key [(f5)] 'revert-buffer)
(global-set-key [(f6)] 'swap-windows)

;;Set keybinding for functions in efunc.el
;;(global-set-key [(meta f10)] 'my-ido-find-tag)
(global-set-key (kbd "C-x f") 'ffip)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer-other-window)
(global-set-key (kbd "C-x M-b") 'ibuffer)


(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window

(global-set-key (kbd "\C-x 5") 'xsteve-split-window)

;; (global-set-key [(C-return)] 'dabbrev-expand)


;; Misc keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(global-set-key [(meta f10)] 'ido-goto-symbol)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "C-c [") 'beginning-of-defun)
(global-set-key (kbd "C-c ]") 'end-of-defun)

(global-set-key [f9] 'pn-open-todays-entry)
(global-set-key (kbd "C-<f9>") 'deft)

(provide 'james-bindings)
