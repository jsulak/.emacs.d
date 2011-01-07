(require 'remember)
(org-remember-insinuate)

;; Org-mode settings

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(global-font-lock-mode 1)
(setq org-hide-leading-stars t)
;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)))

;(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)") 
; (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
; (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;(setq org-todo-state-tags-triggers
;      (quote (("CANCELLED" ("CANCELLED" . t))
;              ("WAITING" ("WAITING" . t) ("NEXT"))
;              ("SOMEDAY" ("WAITING" . t))
;              (done ("NEXT") ("WAITING"))
;              ("TODO" ("WAITING") ("CANCELLED"))
;              ("STARTED" ("WAITING") ("NEXT" . t)))))

;; Change task state to STARTED when clocking in
;(setq org-clock-in-switch-to-state "STARTED")

;(setq org-agenda-custom-commands
;      (quote (("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
;              ("w" "Tasks waiting on something" tags "WAITING/!" ((org-use-tag-inheritance nil)))
;              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE" ((org-agenda-todo-ignore-with-date nil);))
 ;             ("N" "Notes" tags "NOTE" nil)
 ;;             ("n" "Next" tags "NEXT-WAITING-CANCELLED/!" nil))))


;; Remember mode settings
;(setq org-default-notes-file "~/Dropbox/notes/refile.org")

;; Start clock if a remember buffer includes :CLOCK-IN:
;(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

;(defun my-start-clock-if-needed ()
;  (save-excursion
;    (goto-char (point-min))
;    (when (re-search-forward " *:CLOCK-IN: *" nil t)
;      (replace-match "")
;      (org-clock-in))))

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

;; 3 remember templates for TODO tasks, Notes, and Phone calls
;(setq org-remember-templates (quote (("todo" ?t "* TODO %?
;  %u
;  %a" nil bottom nil)
;                                     ("note" ?n "* %?                                        :NOTE:
;  %u
 ;; %a" nil bottom nil)
 ;                                    ("phone" ?p "* PHONE %:name - %:company -                :PHONE:
 ; Contact Info: %a
 ; %u
  ;:CLOCK-IN:
  ;%?" nil bottom nil))))


; Use IDO for target completion
;(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
;(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
;(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
;(setq org-outline-path-complete-in-steps t)

