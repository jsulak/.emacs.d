(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "M-;") 'hippie-expand)

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

;;Set ctrl-z to undo
(global-set-key "\C-z" 'undo)

;;set up kill word keyboard bindings
(global-set-key "\C-w" 'kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-q" 'backward-kill-word)

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

;; Point stack bindings
(global-set-key '[(f8)] 'point-stack-push)
(global-set-key '[(f9)] 'point-stack-pop)
(global-set-key '[(f10)] 'point-stack-forward-stack-pop)

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

(global-set-key [(C-return)] 'dabbrev-expand)


;; Misc keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(global-set-key [(meta f10)] 'ido-goto-symbol)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(provide 'james-bindings)