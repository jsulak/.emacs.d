;;;
;; Smart Tab

(defvar smart-tab-using-hippie-expand nil
  "turn this on if you want to use hippie-expand completion.")

(global-set-key [(tab)] 'smart-tab)
 (defun smart-tab ()
      "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
      (interactive)
      (if (minibufferp)
          (unless (minibuffer-complete)
            (dabbrev-expand nil))
        (if mark-active
            (indent-region (region-beginning)
                           (region-end))
          (if (looking-at "\\_>")
              (dabbrev-expand nil)
            (indent-for-tab-command)))))
(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))