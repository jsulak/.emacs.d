;;; annex-light-theme.el --- Light theme based on Annex -*- lexical-binding: t; -*-

;;; Commentary:
;; Warm, minimal light theme derived from the Annex note-taking app.

;;; Code:

(deftheme annex-light "Light theme based on the Annex app.")

(let ((bg-app      "#F5F4EF")
      (bg-list     "#EDECEA")
      (bg-editor   "#FAFAF8")
      (bg-selected "#D4C9B8")
      (fg-primary  "#2C2C2C")
      (fg-secondary "#7A7570")
      (fg-accent   "#8B6914")
      (border      "#D0CBC3")
      (highlight   "#F5E6A3")
      ;; Derived colors for syntax
      (string      "#3A7A3A")
      (keyword     "#8B4513")
      (type        "#5B4BA0")
      (constant    "#B85020")
      (builtin     "#6A5ACD")
      (func-name   "#2E5090")
      (variable    "#7A4500")
      (error-fg    "#CC4444")
      (warning-fg  "#B8860B")
      (success-fg  "#4A8B4A"))

  (custom-theme-set-faces
   'annex-light

   ;; Core
   `(default                          ((t (:foreground ,fg-primary :background ,bg-editor))))
   `(cursor                           ((t (:background ,fg-primary))))
   `(region                           ((t (:background ,bg-selected))))
   `(highlight                        ((t (:background ,highlight))))
   `(hl-line                          ((t (:background ,bg-app))))
   `(fringe                           ((t (:background ,bg-editor))))
   `(vertical-border                  ((t (:foreground ,border))))
   `(border                           ((t (:foreground ,border))))
   `(shadow                           ((t (:foreground ,fg-secondary))))
   `(minibuffer-prompt                ((t (:foreground ,fg-accent :weight bold))))
   `(link                             ((t (:foreground ,fg-accent :underline t))))
   `(link-visited                     ((t (:foreground ,type :underline t))))
   `(escape-glyph                     ((t (:foreground ,constant))))

   ;; Line numbers
   `(line-number                      ((t (:foreground ,fg-secondary :background ,bg-editor))))
   `(line-number-current-line         ((t (:foreground ,fg-primary :background ,bg-app :weight bold))))

   ;; Mode line
   `(mode-line                        ((t (:foreground ,fg-primary :background ,bg-list
                                          :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive               ((t (:foreground ,fg-secondary :background ,bg-app
                                          :box (:line-width 1 :color ,border)))))
   `(mode-line-emphasis               ((t (:weight bold))))
   `(mode-line-highlight              ((t (:box (:line-width 1 :color ,fg-accent)))))
   `(mode-line-buffer-id              ((t (:weight bold))))
   `(header-line                      ((t (:foreground ,fg-primary :background ,bg-list
                                          :box (:line-width 1 :color ,border)))))

   ;; Font lock (syntax highlighting)
   `(font-lock-comment-face           ((t (:foreground ,fg-secondary :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-secondary :slant italic))))
   `(font-lock-doc-face               ((t (:foreground ,fg-secondary))))
   `(font-lock-string-face            ((t (:foreground ,string))))
   `(font-lock-keyword-face           ((t (:foreground ,keyword :weight bold))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-function-name-face     ((t (:foreground ,func-name :weight bold))))
   `(font-lock-variable-name-face     ((t (:foreground ,variable))))
   `(font-lock-type-face              ((t (:foreground ,type))))
   `(font-lock-constant-face          ((t (:foreground ,constant))))
   `(font-lock-warning-face           ((t (:foreground ,warning-fg :weight bold))))
   `(font-lock-negation-char-face     ((t (:foreground ,error-fg))))
   `(font-lock-preprocessor-face      ((t (:foreground ,constant))))

   ;; Search
   `(isearch                          ((t (:foreground ,fg-primary :background ,highlight :weight bold))))
   `(isearch-fail                     ((t (:foreground ,bg-editor :background ,error-fg))))
   `(lazy-highlight                   ((t (:background ,highlight))))

   ;; Completion
   `(completions-common-part          ((t (:foreground ,fg-accent))))
   `(completions-first-difference     ((t (:weight bold))))
   `(icomplete-first-match            ((t (:foreground ,fg-accent :weight bold))))

   ;; Error / Warning / Success
   `(error                            ((t (:foreground ,error-fg :weight bold))))
   `(warning                          ((t (:foreground ,warning-fg :weight bold))))
   `(success                          ((t (:foreground ,success-fg :weight bold))))

   ;; Paren matching
   `(show-paren-match                 ((t (:background ,bg-selected :weight bold))))
   `(show-paren-mismatch              ((t (:foreground ,bg-editor :background ,error-fg))))

   ;; Whitespace
   `(trailing-whitespace              ((t (:background ,error-fg))))

   ;; Org mode
   `(org-hide                         ((t (:foreground ,bg-editor))))
   `(org-indent                       ((t (:foreground ,bg-editor))))
   `(org-level-1                      ((t (:foreground ,fg-primary :weight bold :height 1.3))))
   `(org-level-2                      ((t (:foreground ,fg-primary :weight bold :height 1.15))))
   `(org-level-3                      ((t (:foreground ,fg-primary :weight bold :height 1.05))))
   `(org-level-4                      ((t (:foreground ,fg-primary :weight bold))))
   `(org-level-5                      ((t (:foreground ,fg-primary :weight bold))))
   `(org-level-6                      ((t (:foreground ,fg-primary :weight bold))))
   `(org-level-7                      ((t (:foreground ,fg-primary :weight bold))))
   `(org-level-8                      ((t (:foreground ,fg-primary :weight bold))))
   `(org-document-title               ((t (:foreground ,fg-primary :weight bold))))
   `(org-document-info                ((t (:foreground ,fg-secondary))))
   `(org-document-info-keyword        ((t (:foreground ,fg-secondary))))
   `(org-todo                         ((t (:foreground ,error-fg :weight bold))))
   `(org-done                         ((t (:foreground ,success-fg :weight bold))))
   `(org-headline-done                ((t (:foreground ,fg-secondary))))
   `(org-date                         ((t (:foreground ,fg-accent :underline t))))
   `(org-link                         ((t (:foreground ,fg-accent :underline t))))
   `(org-tag                          ((t (:foreground ,fg-secondary :weight normal))))
   `(org-block                        ((t (:background ,bg-app))))
   `(org-block-begin-line             ((t (:foreground ,fg-secondary :background ,bg-list))))
   `(org-block-end-line               ((t (:foreground ,fg-secondary :background ,bg-list))))
   `(org-meta-line                    ((t (:foreground ,fg-secondary))))
   `(org-code                         ((t (:foreground ,constant :background ,bg-app))))
   `(org-verbatim                     ((t (:foreground ,builtin :background ,bg-app))))
   `(org-table                        ((t (:foreground ,fg-primary :background ,bg-app))))
   `(org-checkbox                     ((t (:foreground ,fg-accent :weight bold))))
   `(org-agenda-structure             ((t (:foreground ,fg-primary :weight bold :height 1.1))))
   `(org-agenda-date                  ((t (:foreground ,fg-primary :weight bold))))
   `(org-agenda-date-today            ((t (:foreground ,fg-accent :weight bold))))
   `(org-agenda-date-weekend          ((t (:foreground ,fg-secondary :weight bold))))
   `(org-scheduled                    ((t (:foreground ,fg-primary))))
   `(org-scheduled-today              ((t (:foreground ,fg-accent :weight bold))))
   `(org-scheduled-previously         ((t (:foreground ,warning-fg))))
   `(org-upcoming-deadline            ((t (:foreground ,warning-fg))))

   ;; Dired
   `(dired-directory                  ((t (:foreground ,fg-accent :weight bold))))
   `(dired-symlink                    ((t (:foreground ,type))))
   `(dired-ignored                    ((t (:foreground ,fg-secondary))))

   ;; Diff / Magit
   `(diff-added                       ((t (:foreground ,success-fg :background "#E8F0E0"))))
   `(diff-removed                     ((t (:foreground ,error-fg :background "#F0E0E0"))))
   `(diff-header                      ((t (:foreground ,fg-primary :background ,bg-list :weight bold))))
   `(diff-file-header                 ((t (:foreground ,fg-primary :background ,bg-list :weight bold))))
   `(diff-hunk-header                 ((t (:foreground ,fg-accent :background ,bg-app))))

   ;; Tab bar
   `(tab-bar                          ((t (:foreground ,fg-secondary :background ,bg-list))))
   `(tab-bar-tab                      ((t (:foreground ,fg-primary :background ,bg-editor
                                          :box (:line-width 1 :color ,border)))))
   `(tab-bar-tab-inactive             ((t (:foreground ,fg-secondary :background ,bg-list
                                          :box (:line-width 1 :color ,border)))))))

(provide-theme 'annex-light)

;;; annex-light-theme.el ends here
