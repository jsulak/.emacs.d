;;; james-org-readable.el --- Publish Org PDFs for phone reading -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone batch exporter; invoked by bin/export-org-readable.sh without
;; loading the interactive configuration.  PDFs are disposable viewing copies.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox-latex)
(require 'subr-x)

(defconst james/org-readable--library-file
  (or load-file-name buffer-file-name)
  "Exporter source used to invalidate cached PDFs after template changes.")

(defconst james/org-readable--preamble
  "\\documentclass[12pt]{article}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\usepackage{fontspec,graphicx,geometry,longtable,array,booktabs,amsmath,amssymb,pdflscape}
\\usepackage[normalem]{ulem}
\\usepackage{xcolor}
\\usepackage[unicode,breaklinks]{hyperref}
\\geometry{paperwidth=105mm,paperheight=148mm,margin=8mm,bottom=11mm,footskip=6mm}
\\setmainfont{texgyreheros}[Extension=.otf,UprightFont=*-regular,BoldFont=*-bold,ItalicFont=*-italic,BoldItalicFont=*-bolditalic]
\\setsansfont{texgyreheros}[Extension=.otf,UprightFont=*-regular,BoldFont=*-bold,ItalicFont=*-italic,BoldItalicFont=*-bolditalic]
\\setmonofont{texgyrecursor}[Extension=.otf,UprightFont=*-regular,BoldFont=*-bold,ItalicFont=*-italic,BoldItalicFont=*-bolditalic]
\\hypersetup{colorlinks=true,urlcolor=blue!65!black,linkcolor=blue!65!black,bookmarksnumbered=false}
\\urlstyle{same}
\\makeatletter
\\g@addto@macro\\UrlBreaks{\\do\\a\\do\\b\\do\\c\\do\\d\\do\\e\\do\\f\\do\\g\\do\\h\\do\\i\\do\\j\\do\\k\\do\\l\\do\\m\\do\\n\\do\\o\\do\\p\\do\\q\\do\\r\\do\\s\\do\\t\\do\\u\\do\\v\\do\\w\\do\\x\\do\\y\\do\\z\\do\\A\\do\\B\\do\\C\\do\\D\\do\\E\\do\\F\\do\\G\\do\\H\\do\\I\\do\\J\\do\\K\\do\\L\\do\\M\\do\\N\\do\\O\\do\\P\\do\\Q\\do\\R\\do\\S\\do\\T\\do\\U\\do\\V\\do\\W\\do\\X\\do\\Y\\do\\Z\\do\\0\\do\\1\\do\\2\\do\\3\\do\\4\\do\\5\\do\\6\\do\\7\\do\\8\\do\\9}
\\renewcommand\\section{\\@startsection{section}{1}{0pt}{1.5ex}{.6ex}{\\normalfont\\Large\\bfseries}}
\\renewcommand\\subsection{\\@startsection{subsection}{2}{0pt}{1.3ex}{.5ex}{\\normalfont\\large\\bfseries}}
\\renewcommand\\subsubsection{\\@startsection{subsubsection}{3}{0pt}{1ex}{.4ex}{\\normalfont\\normalsize\\bfseries}}
\\makeatother
\\tracinglostchars=3
\\setlength{\\leftmargini}{1.2em}
\\setlength{\\leftmarginii}{1em}
\\setlength{\\leftmarginiii}{1em}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{.45em}
\\setlength{\\tabcolsep}{3pt}
\\renewcommand{\\arraystretch}{1.15}
\\linespread{1.08}
\\raggedright
\\setlength{\\emergencystretch}{2em}
\\pagestyle{plain}"
  "Shared phone-sized PDF style with embedded proportional fonts.")

(defun james/org-readable--plain-text (text info)
  "Export TEXT using INFO, with printable equivalents for note symbols."
  (let ((result (org-latex-plain-text text info)))
    (dolist (pair '(("🚩" . "\\textcolor{red!70!black}{[!]}")
                    ("❓" . "[?]") ("📊" . "[Spreadsheet]")
                    ("📈" . "[Chart]") ("📄" . "[Document]") ("📕" . "[PDF]")
                    ("○" . "\\(\\circ\\)") ("□" . "\\(\\square\\)")
                    ("→" . "\\(\\rightarrow\\)") ("↓" . "\\(\\downarrow\\)")
                    ("≤" . "\\(\\leq\\)") ("≥" . "\\(\\geq\\)")
                    ("−" . "\\(-\\)") ("➢" . "\\(\\triangleright\\)")
                    ("‑" . "-") ("\u200b" . "") ("\u202f" . " ")
                    ("\u2002" . " ")))
      (setq result (string-replace (car pair) (cdr pair) result)))
    ;; Attachment labels often contain long underscore-separated filenames.
    (string-replace "\\_" "\\_\\allowbreak{}" result)))

(defun james/org-readable--image-p (link)
  "Return non-nil for a local raster image LINK."
  (and (equal (org-element-property :type link) "file")
       (member (downcase (or (file-name-extension
                             (org-element-property :path link)) ""))
               '("png" "jpg" "jpeg"))))

(defun james/org-readable--link (link description info)
  "Export LINK and DESCRIPTION using INFO, resolving images at their source."
  (cond
   ;; Imported web links often repeat the entire URL as their description.
   ;; Use LaTeX's breakable URL rendering instead of one unbreakable label.
   ((and (member (org-element-property :type link) '("http" "https"))
         (equal (org-element-property :raw-link link)
                (org-element-interpret-data (org-element-contents link))))
    (org-latex-link link nil info))
   ((and (james/org-readable--image-p link) (not description))
    (let ((path (expand-file-name (org-element-property :path link))))
      (unless (file-regular-p path) (error "Missing image: %s" path))
      ;; Use absolute paths because TeX builds outside the Org collection.
      (org-element-put-property link :path path)
      (org-latex-link link description info)))
   ((equal (org-element-property :type link) "file")
    ;; Local filesystem URLs cannot reliably navigate OneDrive's mobile viewer.
    ;; Keep a human-readable location instead of a misleading clickable link.
    (let* ((path (org-element-property :path link))
           (label (format "\\texorpdfstring{\\nolinkurl{%s}}{%s}"
                          path (james/org-readable--plain-text path info))))
      (if description (format "%s (%s)" description label) label)))
   (t (org-latex-link link description info))))

(defun james/org-readable--headline (headline contents info)
  "Export HEADLINE with CONTENTS and INFO, including unnumbered bookmarks."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (command (nth (min 4 (1- level))
                         '("section" "subsection" "subsubsection"
                           "paragraph" "subparagraph")))
           (todo (and (plist-get info :with-todo-keywords)
                      (org-element-property :todo-keyword headline)))
           (title (org-export-data (org-element-property :title headline) info))
           (heading (funcall (plist-get info :latex-format-headline-function)
                             todo (org-element-property :todo-type headline)
                             nil title nil info)))
      (concat (format "\\%s*{%s}\n" command heading)
              (org-latex--label headline info t t)
              (format "\\addcontentsline{toc}{%s}{%s}\n" command heading)
              contents))))

(defun james/org-readable--plain-list (plain-list contents info)
  "Export PLAIN-LIST and CONTENTS using INFO, limiting visual nesting.
Keep all items but flatten list levels deeper than three for phone reading
and to avoid LaTeX's fixed list nesting limit."
  (if (>= (cl-count 'plain-list (org-element-lineage plain-list)
                   :key #'org-element-type) 3)
      contents
    (org-latex-plain-list plain-list contents info)))

(defun james/org-readable--table (table contents info)
  "Export TABLE with CONTENTS and INFO using wrapping, equal-width columns."
  (let* ((row (org-element-map table 'table-row
                (lambda (item)
                  (and (eq (org-element-property :type item) 'standard) item))
                info t))
         (columns (length (org-element-contents row))))
    (when (> columns 0)
      (org-element-put-property
       table :attr_latex
       (list (concat ":environment longtable :float nil :align "
                     (mapconcat
                      #'identity
                      (make-list columns
                                 (format ">{\\raggedright\\arraybackslash}p{\\dimexpr\\linewidth/%d-2\\tabcolsep\\relax}"
                                         columns))
                      "")))))
    (concat (when (>= columns 4) "\\begin{landscape}\n")
            "{\\small\n" (org-latex-table table contents info) "\n}\n"
            (when (>= columns 4) "\\end{landscape}\n"))))

(org-export-define-derived-backend 'james-readable 'latex
  :translate-alist '((plain-text . james/org-readable--plain-text)
                     (link . james/org-readable--link)
                     (headline . james/org-readable--headline)
                     (plain-list . james/org-readable--plain-list)
                     (table . james/org-readable--table)))

(defun james/org-readable--latex (source)
  "Return a styled LaTeX document for the current Org buffer from SOURCE."
  (let ((org-export-use-babel nil)
        (org-export-before-processing-functions nil)
        (org-export-before-parsing-functions nil)
        (org-export-filter-final-output-functions nil)
        (org-latex-classes
         `(("james-readable" ,james/org-readable--preamble
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
    (org-export-as
     'james-readable nil nil nil
     (list :latex-class "james-readable" :latex-compiler "xelatex"
           :title (or (cadar (org-collect-keywords '("TITLE")))
                      (file-name-base source))
           :with-author nil :with-email nil :with-creator nil :with-date nil
           :with-toc nil :section-numbers nil :headline-levels 5
           :with-todo-keywords t :with-tags nil :with-priority nil
           :with-drawers nil :with-properties nil :with-clocks nil
           :with-planning t :with-sub-superscript nil :with-broken-links t
           :time-stamp-file nil :latex-tables-booktabs t
           :latex-title-command "{\\LARGE\\bfseries %t\\par}\\medskip"
           :latex-image-default-width "\\linewidth"
           :latex-image-default-height "0.75\\textheight"
           :latex-image-default-option "keepaspectratio"
           :latex-default-figure-position "htbp"))))

(defun james/org-readable--file-hash (file)
  "Return a content digest for FILE, or signal an error if it is unavailable."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun james/org-readable--fingerprint (source style-hash)
  "Hash SOURCE, its image dependencies, and STYLE-HASH in the Org buffer."
  ;; Hash the captured buffer, so a save during export cannot cache a newer
  ;; on-disk revision against a PDF built from older text.
  (let ((parts (list style-hash source (secure-hash 'sha256 (current-buffer))))
        images)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (james/org-readable--image-p link)
          (push (expand-file-name (org-element-property :path link)) images))))
    (dolist (image (sort (delete-dups images) #'string<))
      (push image parts)
      (push (james/org-readable--file-hash image) parts))
    (secure-hash 'sha256 (prin1-to-string parts))))

(defun james/org-readable--publish (source output log-file)
  "Compile current Org buffer from SOURCE to OUTPUT, logging to LOG-FILE."
  (let* ((build (make-temp-file "org-readable-build-" t))
         (tex (expand-file-name "document.tex" build))
         (pdf (expand-file-name "document.pdf" build))
         (log-buffer (generate-new-buffer " *org-readable-tex*"))
         temporary-output)
    (unwind-protect
        (progn
          (let ((document (james/org-readable--latex source)))
            (with-temp-file tex (insert document)))
          (let ((default-directory build))
            (dotimes (_ 2)
              (unless (zerop (call-process
                             "xelatex" nil log-buffer nil
                             "-interaction=nonstopmode" "-halt-on-error"
                             "-file-line-error" "-no-shell-escape" tex))
                (error "XeLaTeX failed; see %s" log-file))))
          (unless (file-regular-p pdf) (error "XeLaTeX produced no PDF"))
          (make-directory (file-name-directory output) t)
          (setq temporary-output
                (make-temp-file (expand-file-name ".readable-"
                                                  (file-name-directory output))))
          (copy-file pdf temporary-output t)
          (rename-file temporary-output output t))
      (with-current-buffer log-buffer
        (write-region (point-min) (point-max) log-file nil 'silent))
      (kill-buffer log-buffer)
      (when (and temporary-output (file-exists-p temporary-output))
        (delete-file temporary-output))
      (delete-directory build t))))

(defun james/org-readable--output (relative destination)
  "Return a safe PDF path for RELATIVE Org filename under DESTINATION."
  (unless (and (not (file-name-absolute-p relative))
               (not (member ".." (split-string relative "/")))
               (string-suffix-p ".org" relative))
    (error "Invalid relative Org filename: %s" relative))
  (let ((output (expand-file-name (concat (file-name-sans-extension relative) ".pdf")
                                 destination))
        (directory (file-name-as-directory destination)))
    (dolist (component (split-string (file-relative-name output directory) "/"))
      (setq directory (expand-file-name component directory))
      (when (file-symlink-p directory)
        (error "Readable destination must not contain symlinks: %s" directory)))
    output))

(defun james/org-readable-export (source destination state-directory &optional selected)
  "Publish SOURCE Org files into DESTINATION, caching in STATE-DIRECTORY.
Optional SELECTED is a list of relative Org filenames for a partial run.
Return the number of failures.  A full run removes only previously managed
PDFs whose source files no longer exist."
  (setq source (file-name-as-directory (expand-file-name source))
        destination (file-name-as-directory (expand-file-name destination)))
  (unless (file-directory-p source) (error "Missing Org directory: %s" source))
  (when (or (file-symlink-p (directory-file-name destination))
            (file-in-directory-p destination source))
    (error "Readable destination must be outside the source and not a symlink"))
  (unless (executable-find "xelatex") (error "XeLaTeX is not on PATH"))
  (make-directory destination t)
  (make-directory state-directory t)
  (let* ((manifest (expand-file-name "manifest.el" state-directory))
         (style-hash (james/org-readable--file-hash james/org-readable--library-file))
         (files (or selected
                    (mapcar (lambda (file) (file-relative-name file source))
                            (directory-files-recursively source "\\.org\\'"))))
         (updated 0) (unchanged 0) (failed 0) (removed 0)
         entries)
    (lock-file manifest)
    (unwind-protect
        (progn
          (when (file-exists-p manifest)
            (with-temp-buffer
              (insert-file-contents manifest)
              (setq entries (read (current-buffer)))))
          (dolist (relative files)
            (condition-case problem
                (let* ((file (expand-file-name relative source))
                       (output (james/org-readable--output relative destination))
                       (log-file (expand-file-name
                                  (concat (secure-hash 'sha256 relative) ".log")
                                  state-directory))
                       (default-directory (file-name-directory file))
                       (org-todo-keywords
                        '((sequence "TODO" "WAITING" "ON HOLD" "|" "DONE" "OBE"))))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (setq buffer-file-name file)
                    (delay-mode-hooks (org-mode))
                    (let ((fingerprint (james/org-readable--fingerprint file style-hash)))
                      (if (and (equal (cdr (assoc relative entries)) fingerprint)
                               (file-regular-p output))
                          (cl-incf unchanged)
                        (james/org-readable--publish file output log-file)
                        (setf (alist-get relative entries nil nil #'equal) fingerprint)
                        (cl-incf updated)
                        (message "Readable: %s" relative)))))
              (error
               (cl-incf failed)
               (message "Readable FAILED: %s: %s" relative
                        (error-message-string problem)))))
          (unless selected
            (dolist (entry (copy-sequence entries))
              (unless (member (car entry) files)
                (let ((output (james/org-readable--output (car entry) destination)))
                  (when (file-exists-p output) (delete-file output)))
                (setq entries (assoc-delete-all (car entry) entries))
                (cl-incf removed))))
          (let ((temporary (make-temp-file (expand-file-name ".manifest-" state-directory))))
            (unwind-protect
                (progn
                  (with-temp-file temporary (prin1 entries (current-buffer)) (insert "\n"))
                  (rename-file temporary manifest t))
              (when (file-exists-p temporary) (delete-file temporary))))
          (message "Readable: %d updated, %d unchanged, %d removed, %d failed"
                   updated unchanged removed failed)
          failed)
      (unlock-file manifest))))

(defun james/org-readable-batch ()
  "Run the exporter using environment paths and optional relative filenames."
  (let* ((source (or (getenv "ORG_SOURCE_DIR")
                     (error "Set ORG_SOURCE_DIR before running the batch exporter")))
         (backup (expand-file-name
                  "org-backups"
                  (or (getenv "ORG_BACKUP_DESTINATION")
                      (error "Set ORG_BACKUP_DESTINATION before running the batch exporter"))))
         (selected (delete "--" command-line-args-left)))
    (setq command-line-args-left nil)
    (kill-emacs
     (if (zerop (james/org-readable-export
                 source (expand-file-name "readable" backup)
                 (expand-file-name ".state/readable" backup) selected)) 0 1))))

(provide 'james-org-readable)
;;; james-org-readable.el ends here
