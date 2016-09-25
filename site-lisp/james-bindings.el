;; http://whattheemacsd.com//editing-defuns.el-01.html
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)


;; Multiple cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Rectangular region mode
(global-set-key (kbd "C-c SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c C-SPC") 'set-rectangular-region-anchor)

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

;; Jump to a definition in the current file.
(global-set-key (kbd "C-x C-i") 'imenu)

;; Run shell command
(global-set-key (kbd "\C-c s") 'shell-command)

;; Set goto-line
(global-set-key "\C-x\C-g" 'goto-line)

;;bind repeat last macro to F5
(global-set-key [(f4)] 'call-last-kbd-macro)
(global-set-key [(f5)] 'revert-buffer)
(global-set-key [(ctrl f5)] 'revert-buffer-no-confirm)
(global-set-key [(f6)] 'swap-windows)

;; vc-diff
(global-set-key [(f7)] 'vc-diff)
(global-set-key [(f8)] 'indent-region)

;;Set keybinding for functions in efunc.el
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

;; Git gutter keybindings
;;; Jump between hunks
(global-set-key (kbd "C-x n") 'git-gutter+-next-hunk)
(global-set-key (kbd "C-x p") 'git-gutter+-previous-hunk)

;;; Act on hunks
(global-set-key (kbd "C-x v =") 'git-gutter+-popup-hunk) ; Show detailed diff
(global-set-key (kbd "C-x v r") 'git-gutter+-revert-hunk)
;; Stage hunk at point.
;; If region is active, stage all hunk lines within the region.
(global-set-key (kbd "C-x t") 'git-gutter+-stage-hunks)
(global-set-key (kbd "C-x c") 'git-gutter+-commit) ; Commit with Magit
(global-set-key (kbd "C-x C") 'git-gutter+-stage-and-commit)

(global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
(global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

;; From http://irreal.org/blog/?p=5585
(defun jcs-kill-a-buffer (askp)
  (interactive "P")
  (if askp
      (kill-buffer (funcall completing-read-function
                            "Kill buffer: "
                            (mapcar #'buffer-name (buffer-list))))
    (kill-this-buffer)))
(global-set-key (kbd "C-x k") 'jcs-kill-a-buffer)


(provide 'james-bindings)
