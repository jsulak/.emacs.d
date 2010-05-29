;;; acl-mode.el --- Adept Command Language editing commands for GNU Emacs

;;
;; RefPurpose: An Emacs major-mode for editing ACL
;;
;; $Id: acl-mode.el 1.3 1999/07/13 12:58:10 nwalsh Exp nwalsh $
;;

;; Copyright (C) 1997, 1998 ArborText, Inc.

;; Author: Norman Walsh <nwalsh@arbortext.com>
;; Maintainer: nwalsh
;; Keywords: languages

;; Adapted from perl code editing commands 'perl-mode.el', 
;; Copyright 1990, 1994 by the Free Software Foundation, under terms 
;; of its General Public License.

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To enter acl-mode automatically, add (autoload 'acl-mode "acl-mode")
;; to your .emacs file and change the first line of your ACL script to:
;; # -*- ACL -*-
;; To handle files automatically by extension, add something like
;; (setq auto-mode-alist (append (list (cons "\\.cmd\\'" 'acl-mode))
;;                               auto-mode-alist))
;; to your .emacs file.

;; If your machine is slow, you may want to remove some of the bindings
;; to electric-acl-terminator.

;; Known problems (these are all caused by limitations in the Emacs Lisp
;; parsing routine (parse-partial-sexp), which was not designed for such
;; a rich language; writing a more suitable parser would be a big job):
;; 1)  Regular expression delimiters do not act as quotes, so special
;;       characters such as `'"#:;[](){} may need to be backslashed
;;       in regular expressions and in both parts of s/// and tr///.
;; 2)  \ (backslash) always quotes the next character, so '\' is
;;       treated as the start of a string.  Use "\\" as a work-around.
;; 3)  To make variables such a $' and $#array work, perl-mode treats
;;       $ just like backslash, so '$' is the same as problem 2.
;; 4)  Unfortunately, treating $ like \ makes ${var} be treated as an
;;       unmatched }.  See below.

;;; Code:

(defvar acl-mode-abbrev-table nil
  "Abbrev table in use in acl-mode buffers.")
(define-abbrev-table 'acl-mode-abbrev-table ())

(defvar acl-mode-map ()
  "Keymap used in ACL mode.")
(if acl-mode-map
    ()
  (setq acl-mode-map (make-sparse-keymap))
  (define-key acl-mode-map "{" 'electric-acl-terminator)
  (define-key acl-mode-map "}" 'electric-acl-terminator)
  (define-key acl-mode-map ";" 'electric-acl-terminator)
  (define-key acl-mode-map ":" 'electric-acl-terminator)
  (define-key acl-mode-map "\e\C-a" 'acl-beginning-of-function)
  (define-key acl-mode-map "\e\C-e" 'acl-end-of-function)
  (define-key acl-mode-map "\e\C-h" 'mark-acl-function)
  (define-key acl-mode-map "\e\C-q" 'indent-acl-exp)
  (define-key acl-mode-map "\177" 'backward-delete-char-untabify)
  (define-key acl-mode-map "\t" 'acl-indent-command))

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)

(defvar acl-mode-syntax-table nil
  "Syntax table in use in acl-mode buffers.")

(if acl-mode-syntax-table
    ()
  (setq acl-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n ">" acl-mode-syntax-table)
  (modify-syntax-entry ?# "<" acl-mode-syntax-table)
  (modify-syntax-entry ?$ "/" acl-mode-syntax-table)
  (modify-syntax-entry ?% "." acl-mode-syntax-table)
  (modify-syntax-entry ?& "." acl-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" acl-mode-syntax-table)
  (modify-syntax-entry ?* "." acl-mode-syntax-table)
  (modify-syntax-entry ?+ "." acl-mode-syntax-table)
  (modify-syntax-entry ?- "." acl-mode-syntax-table)
  (modify-syntax-entry ?/ "." acl-mode-syntax-table)
  (modify-syntax-entry ?< "." acl-mode-syntax-table)
  (modify-syntax-entry ?= "." acl-mode-syntax-table)
  (modify-syntax-entry ?> "." acl-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" acl-mode-syntax-table)
  (modify-syntax-entry ?` "\"" acl-mode-syntax-table)
  (modify-syntax-entry ?| "." acl-mode-syntax-table)
)

(defvar acl-imenu-generic-expression
  '(
    ;; Functions
    (nil "^function\\s-+\\([-A-Za-z0-9+_:]+\\)\\(\\s-\\|\n\\)*{" 1 )
    ;;Variables
    ("Variables" "^\\([$@%][-A-Za-z0-9+_:]+\\)\\s-*=" 1 )
    ("Packages" "^package\\s-+\\([-A-Za-z0-9+_:]+\\);" 1 )
    )
  "Imenu generic expression for Acl mode.  See `imenu-generic-expression'.")

;; Regexps updated with help from Tom Tromey <tromey@cambric.colorado.edu> and
;; Jim Campbell <jec@murzim.ca.boeing.com>.

(defconst acl-font-lock-keywords-1
  '(;; What is this for?
    ;;("\\(--- .* ---\\|=== .* ===\\)" . font-lock-string-face)
    ;;
    ;; Fontify preprocessor statements as we do in `c-font-lock-keywords'.
    ;; Ilya Zakharevich <ilya@math.ohio-state.edu> thinks this is a bad idea.
    ("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
    ("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
    ("^#[ \t]*if\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
    ("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t))
    ;;
    ;; Fontify function and package names in declarations.
    ("\\<\\(package\\|function\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ("\\<\\(import\\|no\\|require\\|use\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-reference-face nil t)))
  "Subdued level highlighting for Acl mode.")

(defconst acl-font-lock-keywords-2
  (append acl-font-lock-keywords-1
   (list
    ;;
    ;; Fontify keywords, except those fontified otherwise.
;   (make-regexp '("if" "while" "else" "for" "foreach" "exit"
;   "return" "exec"))
    (concat "\\<\\("
	    "e\\(lse\\|x\\(ec\\|it\\)\\)\\|for\\(\\|each\\)\\|"
	    "if\\|return\\|while"
	    "\\)\\>")
    ;;
    ;; Fontify local and global as types.
    '("\\<\\(local\\|global\\)\\>" . font-lock-type-face)
    ;;
    ;; Fontify function, variable and file name references.
    '("&\\(\\sw+\\)" 1 font-lock-function-name-face)
    ;; Additionally underline non-scalar variables.  Maybe this is a bad idea.
    ;;'("[$@%*][#{]?\\(\\sw+\\)" 1 font-lock-variable-name-face)
    '("[$*]{?\\(\\sw+\\)" 1 font-lock-variable-name-face)
    '("\\([@%]\\|\\$#\\)\\(\\sw+\\)"
      (2 (cons font-lock-variable-name-face '(underline))))
    '("<\\(\\sw+\\)>" 1 font-lock-reference-face)
    ;;
    ;; Fontify keywords with/and labels as we do in `c++-font-lock-keywords'.
    '("\\<\\(continue\\|goto\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-reference-face)))
  "Gaudy level highlighting for Acl mode.")

(defvar acl-font-lock-keywords acl-font-lock-keywords-1
  "Default expressions to highlight in Acl mode.")

(defvar acl-indent-level 2
  "*Indentation of Acl statements with respect to containing block.")
(defvar acl-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")
(defvar acl-continued-brace-offset -2
  "*Extra indent for substatements that start with open-braces.
This is in addition to `acl-continued-statement-offset'.")
(defvar acl-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")
(defvar acl-brace-imaginary-offset 0
  "*Imagined indentation of an open brace that actually follows a statement.")
(defvar acl-label-offset 0
  "*There are no labels in ACL so this is meaningless.")

(defvar acl-tab-always-indent t
  "*Non-nil means TAB in Acl mode always indents the current line.
Otherwise it inserts a tab character if you type it past the first
nonwhite character on the line.")

;; I changed the default to nil for consistency with general Emacs
;; conventions -- rms.
(defvar acl-tab-to-comment nil
  "*Non-nil means TAB moves to eol or makes a comment in some cases.
For lines which don't need indenting, TAB either indents an
existing comment, moves to end-of-line, or if at end-of-line already,
create a new comment.")

(defvar acl-nochange ";?#\\|\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:"
  "*Lines starting with this regular expression are not auto-indented.")

;;;###autoload
(defun acl-mode ()
  "Major mode for editing ACL code.
Expression and list commands understand all Acl brackets.
Tab indents for Acl code.
Comments are delimited with # ... \\n.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{acl-mode-map}
Variables controlling indentation style:
 acl-tab-always-indent
    Non-nil means TAB in Acl mode should always indent the current line,
    regardless of where in the line point is when the TAB command is used.
 acl-tab-to-comment
    Non-nil means that for lines which don't need indenting, TAB will
    either delete an empty comment, indent an existing comment, move 
    to end-of-line, or if at end-of-line already, create a new comment.
 acl-nochange
    Lines starting with this regular expression are not auto-indented.
 acl-indent-level
    Indentation of Acl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 acl-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 acl-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `acl-continued-statement-offset'.
 acl-brace-offset
    Extra indentation for line if it starts with an open brace.
 acl-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 acl-label-offset
    Extra indentation for line that is a label.

Various indentation styles:       K&R  BSD  BLK  GNU  LW
  acl-indent-level                5    8    0    2    4
  acl-continued-statement-offset  5    8    4    2    4
  acl-continued-brace-offset      0    0    0    0   -4
  acl-brace-offset               -5   -8    0    0    0
  acl-brace-imaginary-offset      0    0    4    0    0
  acl-label-offset               -5   -8   -2   -2   -2

Turning on Acl mode runs the normal hook `acl-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map acl-mode-map)
  (setq major-mode 'acl-mode)
  (setq mode-name "Acl")
  (setq local-abbrev-table acl-mode-abbrev-table)
  (set-syntax-table acl-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'acl-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(^\\|\\s-\\);?#+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'acl-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Tell font-lock.el how to handle Acl.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((acl-font-lock-keywords
			      acl-font-lock-keywords-1
			      acl-font-lock-keywords-2)
			     nil nil ((?\_ . "w"))))
  ;; Tell imenu how to handle Acl.
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression acl-imenu-generic-expression)
  (run-hooks 'acl-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Acl code
;; based on its context.
(defun acl-comment-indent ()
  (if (and (bolp) (not (eolp)))
      0					;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (if (bolp)			;Else indent at comment column
	       0			; except leave at least one space if
	     (1+ (current-column)))	; not at beginning of line.
	   comment-column))))

(defun electric-acl-terminator (arg)
  "Insert character and adjust indentation.
If at end-of-line, and not in a comment or a quote, correct the's indentation."
  (interactive "P")
  (let ((insertpos (point)))
    (and (not arg)			; decide whether to indent
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (and (not			; eliminate comments quickly
		 (re-search-forward comment-start-skip insertpos t)) 
		(or (/= last-command-char ?:)
		    ;; Colon is special only after a label ....
		    (looking-at "\\s-*\\(\\w\\|\\s_\\)+$"))
		(let ((pps (parse-partial-sexp 
			    (acl-beginning-of-function) insertpos)))
		  (not (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))
	 (progn				; must insert, indent, delete
	   (insert-char last-command-char 1)
	   (acl-indent-line)
	   (delete-char -1))))
  (self-insert-command (prefix-numeric-value arg)))

;; not used anymore, but may be useful someday:
;;(defun acl-inside-parens-p ()
;;  (condition-case ()
;;      (save-excursion
;;	(save-restriction
;;	  (narrow-to-region (point)
;;			    (acl-beginning-of-function))
;;	  (goto-char (point-max))
;;	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
;;    (error nil)))

(defun acl-indent-command (&optional arg)
  "Indent current line as Acl code, or optionally, insert a tab character.

With an argument, indent the current line, regardless of other options.

If `acl-tab-always-indent' is nil and point is not in the indentation
area at the beginning of the line, simply insert a tab.

Otherwise, indent the current line.  If point was within the indentation
area it is moved to the end of the indentation area.  If the line was
already indented proacly and point was not within the indentation area,
and if `acl-tab-to-comment' is non-nil (the default), then do the first
possible action from the following list:

  1) delete an empty comment
  2) move forward to start of comment, indenting if necessary
  3) move forward to end of line
  4) create an empty comment
  5) move backward to start of comment, indenting if necessary."
  (interactive "P")
  (if arg				; If arg, just indent this line
      (acl-indent-line "\f")
    (if (and (not acl-tab-always-indent)
	     (> (current-column) (current-indentation)))
	(insert-tab)
      (let (bof lsexp delta (oldpnt (point)))
	(beginning-of-line) 
	(setq lsexp (point))
	(setq bof (acl-beginning-of-function))
	(goto-char oldpnt)
	(setq delta (acl-indent-line "\f\\|;?#" bof))
	(and acl-tab-to-comment
	     (= oldpnt (point))		; done if point moved
	     (if (listp delta)		; if line starts in a quoted string
		 (setq lsexp (or (nth 2 delta) bof))
	       (= delta 0))		; done if indenting occurred
	     (let (eol state)
	       (end-of-line) 
	       (setq eol (point))
	       (if (= (char-after bof) ?=)
		   (if (= oldpnt eol)
		       (message "In a format statement"))     
		 (setq state (parse-partial-sexp lsexp eol))
		 (if (nth 3 state)
		     (if (= oldpnt eol)	; already at eol in a string
			 (message "In a string which starts with a %c."
				  (nth 3 state)))
		   (if (not (nth 4 state))
		       (if (= oldpnt eol) ; no comment, create one?
			   (indent-for-comment))
		     (beginning-of-line)
		     (if (re-search-forward comment-start-skip eol 'move)
			 (if (eolp)
			     (progn	; kill existing comment
			       (goto-char (match-beginning 0))
			       (skip-chars-backward " \t")
			       (kill-region (point) eol))
			   (if (or (< oldpnt (point)) (= oldpnt eol))
			       (indent-for-comment) ; indent existing comment
			     (end-of-line)))
		       (if (/= oldpnt eol)
			   (end-of-line)
			 (message "Use backslash to quote # characters.")
			 (ding t))))))))))))

(defun acl-indent-line (&optional nochange parse-start)
  "Indent current line as Acl code.
Return the amount the indentation 
changed by, or (parse-state) if line starts in a quoted string."
  (let ((case-fold-search nil)
	(pos (- (point-max) (point)))
	(bof (or parse-start (save-excursion (acl-beginning-of-function))))
	beg indent shift-amt)
    (beginning-of-line)
    (setq beg (point))
    (setq shift-amt
	  (cond ((= (char-after bof) ?=) 0)
		((listp (setq indent (calculate-acl-indent bof))) indent)
		((looking-at (or nochange acl-nochange)) 0)
		(t
		 (skip-chars-forward " \t\f")
		 (cond ((looking-at "\\(\\w\\|\\s_\\)+:[^:]")
			(setq indent (max 1 (+ indent acl-label-offset))))
		       ((= (following-char) ?})
			(setq indent (- indent acl-indent-level)))
		       ((= (following-char) ?{)
			(setq indent (+ indent acl-brace-offset))))
		 (- indent (current-column)))))
    (skip-chars-forward " \t\f")
    (if (and (numberp shift-amt) (/= 0 shift-amt))
	(progn (delete-region beg (point))
	       (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt))

(defun calculate-acl-indent (&optional parse-start)
  "Return appropriate indentation for current line as Acl code.
In usual case returns an integer: the column to indent to.
Returns (parse-state) if line starts inside a string."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  (colon-line-end 0)
	  state containing-sexp)
      (if parse-start			;used to avoid searching
	  (goto-char parse-start)
	(acl-beginning-of-function))
      (while (< (point) indent-point)	;repeat until right sexp
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
; state = (depth_in_parens innermost_containing_list last_complete_sexp
;          string_terminator_or_nil inside_commentp following_quotep
;          minimum_paren-depth_this_scan)
; Parsing stops if depth in parentheses becomes equal to third arg.
	(setq containing-sexp (nth 1 state)))
      (cond ((nth 3 state) state)	; In a quoted string?
	    ((null containing-sexp)	; Line is at top level.
	     (skip-chars-forward " \t\f")
	     (if (= (following-char) ?{)
		 0   ; move to beginning of line if it starts a function body
	       ;; indent a little if this is a continuation line
	       (acl-backward-to-noncomment)
	       (if (or (bobp)
		       (memq (preceding-char) '(?\; ?\})))
		   0 acl-continued-statement-offset)))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (acl-backward-to-noncomment)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (memq (char-syntax (char-after (- (point) 2)))
				   '(?w ?_))))
	       (if (eq (preceding-char) ?\,)
		   (acl-backward-to-start-of-continued-exp containing-sexp)
		 (beginning-of-line))
	       (acl-backward-to-noncomment))
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) '(?\; ?\} ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  acl-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (acl-backward-to-start-of-continued-exp containing-sexp)
		   (+ acl-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (looking-at "[ \t]*{"))
			  acl-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position at last unclosed open.
	       (goto-char containing-sexp)
	       (or
		 ;; If open paren is in col 0, close brace is special
		 (and (bolp)
		      (save-excursion (goto-char indent-point)
				      (looking-at "[ \t]*}"))
		      acl-indent-level)
		 ;; Is line first statement after an open-brace?
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   ;; Skip over comments and labels following openbrace.
		   (while (progn
			    (skip-chars-forward " \t\f\n")
			    (cond ((looking-at ";?#")
				   (forward-line 1) t)
				  ((looking-at "\\(\\w\\|\\s_\\)+:")
				   (save-excursion 
				     (end-of-line) 
				     (setq colon-line-end (point)))
				   (search-forward ":")))))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(if (> colon-line-end (point))
			    (- (current-indentation) acl-label-offset)
			  (current-column))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open paren in column zero, don't let statement
		 ;; start there too.  If acl-indent-level is zero,
		 ;; use acl-brace-offset + acl-continued-statement-offset
		 ;; For open-braces not the first thing in a line,
		 ;; add in acl-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop acl-indent-level))
			(+ acl-brace-offset acl-continued-statement-offset)
		      acl-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the acl-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 acl-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun acl-backward-to-noncomment ()
  "Move point backward to after the first non-white-space, skipping comments."
  (interactive)
  (let (opoint stop)
    (while (not stop)
      (setq opoint (point))
      (beginning-of-line)
      (if (re-search-forward comment-start-skip opoint 'move 1)
	  (progn (goto-char (match-end 1))
		 (skip-chars-forward ";")))
      (skip-chars-backward " \t\f")
      (setq stop (or (bobp)
		     (not (bolp))
		     (forward-char -1))))))

(defun acl-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t\f"))

;; note: this may be slower than the c-mode version, but I can understand it.
(defun indent-acl-exp ()
  "Indent each line of the Acl grouping following point."
  (interactive)
  (let* ((case-fold-search nil)
	 (oldpnt (point-marker))
	 (bof-mark (save-excursion
		     (end-of-line 2)
		     (acl-beginning-of-function)
		     (point-marker)))
	 eol last-mark lsexp-mark delta)
    (if (= (char-after (marker-position bof-mark)) ?=)
	(message "Can't indent a format statement")
      (message "Indenting Acl expression...")
      (save-excursion (end-of-line) (setq eol (point)))
      (save-excursion			; locate matching close paren
	(while (and (not (eobp)) (<= (point) eol))
	  (parse-partial-sexp (point) (point-max) 0))
	(setq last-mark (point-marker)))
      (setq lsexp-mark bof-mark)
      (beginning-of-line)
      (while (< (point) (marker-position last-mark))
	(setq delta (acl-indent-line nil (marker-position bof-mark)))
	(if (numberp delta)		; unquoted start-of-line?
	    (progn 
	      (if (eolp)
		  (delete-horizontal-space))
	      (setq lsexp-mark (point-marker))))
	(end-of-line)
	(setq eol (point))
	(if (nth 4 (parse-partial-sexp (marker-position lsexp-mark) eol))
	    (progn			; line ends in a comment
	      (beginning-of-line)
	      (if (or (not (looking-at "\\s-*;?#"))
		      (listp delta)
		      (and (/= 0 delta)
			   (= (- (current-indentation) delta) comment-column)))
		  (if (re-search-forward comment-start-skip eol t)
		      (indent-for-comment))))) ; indent existing comment
	(forward-line 1))
      (goto-char (marker-position oldpnt))
      (message "Indenting Acl expression...done"))))

(defun acl-beginning-of-function (&optional arg)
  "Move backward to next beginning-of-function, or as far as possible.
With argument, repeat that many times; negative args move forward.
Returns new value of point in all cases."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-char 1))
  (and (/= arg 0)
       (re-search-backward "^\\s(\\|^\\s-*function\\b[^{]+{\\|^\\s-*format\\b[^=]*=\\|^\\."
			   nil 'move arg)
       (goto-char (1- (match-end 0))))
  (point))

;; note: this routine is adapted directly from emacs lisp.el, end-of-defun;
;; no bugs have been removed :-)
(defun acl-end-of-function (&optional arg)
  "Move forward to next end-of-function.
The end of a function is found by moving forward from the beginning of one.
With argument, repeat that many times; negative args move backward."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (acl-beginning-of-function 1)
			  (not (bobp))))
		    nil
		  (or (bobp) (forward-char -1))
		  (acl-beginning-of-function -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[#\n]")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(acl-beginning-of-function 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (progn (acl-beginning-of-function 2) (not (bobp)))
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[#\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defun mark-acl-function ()
  "Put mark at end of Acl function, point at beginning."
  (interactive)
  (push-mark (point))
  (acl-end-of-function)
  (push-mark (point))
  (acl-beginning-of-function)
  (backward-paragraph))

;;;;;;;; That's all, folks! ;;;;;;;;;
