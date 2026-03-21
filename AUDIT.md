# Emacs Config Audit (2026-03-21)

Evaluated against Emacs 29.3.

## High Priority

- [x] GNU ELPA URL uses HTTP instead of HTTPS (`init.el:20`)
- [x] Bindings reference missing packages: `multiple-cursors`, `git-gutter+`, `deft`, `ffip`, `pn-open-todays-entry` — will error at runtime
- [x] `defadvice` is deprecated — used in `james-functions.el` (slick copy/cut) and `james-windows.el` (grep-compute-defaults). Use `advice-add` / `define-advice`.
- [x] `flet` is deprecated in `ido-goto-symbol` (`james-functions.el:259`). Use `cl-flet`.
- [x] `insert-string` is obsolete in `djcb-duplicate-line` (`james-functions.el:180`). Use `insert`.
- [x] `case` macro is deprecated in `dired-launch-command` (`james-functions.el:154`). Use `pcase` or `cl-case`.
- [x] `first`/`second` are old `cl` functions in `swap-windows` (`james-functions.el:33-34`). Use `car`/`cadr`.
- [x] `smartparens-mode` diminished but not installed (`init.el:272`)

## Medium Priority

- [x] `find-tag` is obsolete (`james-bindings.el:22`). Replaced with `xref-find-definitions`/`xref-go-back`. Removed `etags` require and `my-ido-find-tag`.
- [x] Smex is unmaintained (last update 2015) — replaced with `amx` (drop-in fork).
- [ ] **TODO:** Consider replacing ido + amx with the vertico + consult + orderless + marginalia stack for a more modern completion experience.
- [x] `uniquify-seperator` is a typo (`init.el:180`) — fixed to `uniquify-separator`.
- [x] `pending-delete-mode` (`init.el:208`) is redundant with `delete-selection-mode` — removed.
- [x] Clean up unused files in `external/`: removed `smart-tab.el`, `color-theme-blue.el`, `xquery-mode.el`, `w32-browser.el`, and `jshint-mode` submodule.
- [x] `fset 'yes-or-no-p` hack (`init.el:130`) — replaced with `(setopt use-short-answers t)`.

## Low Priority

- [x] `python -mjson.tool` in `pretty-print-json` — updated to `python3`.
- [x] Adopt `use-package` (built into Emacs 29) for cleaner config management.
- [x] `C-c l` binding for `open-current-buffer-mac` — guarded with `(eq system-type 'darwin)`.
- [x] Unnecessary `dired-jump` autoloads — removed (bundled since Emacs 28).

## Remaining TODOs

- [x] Consider replacing ido + amx with vertico + consult + orderless + marginalia.

## Suggested Modernizations

### High Impact
- [x] Add **magit** for git integration.
- [x] Add **eglot** (built into Emacs 29) for LSP support: completion, diagnostics, rename, etc.
- [x] Add **corfu** (or company-mode) for in-buffer popup completion, pairs well with eglot.
- [x] Replace **undo-tree** with **vundo** — undo-tree has known data-corruption bugs and is largely unmaintained.

### Medium Impact
- [ ] Use **project.el** (built-in) for project-scoped find-file, grep, and compile.
- [x] Move remaining `global-set-key` calls in `james-bindings.el` into `use-package` `:bind` blocks for consistency.
- [x] Install solarized theme from MELPA instead of vendoring it in `external/`.
- [x] Enable `savehist-mode` to persist minibuffer history across sessions.
- [x] Configure or remove `exec-path-from-shell` — configured to run on GUI frames.
- [x] Enable `electric-pair-mode` for automatic delimiter pairing.

### Low Impact
- [ ] Replace `fset` keyboard macros (`indent-all`, `find-todo` in `james-functions.el`) with normal defuns.
- [ ] Enable `pixel-scroll-precision-mode` (Emacs 29) for smooth scrolling.
- [ ] Enable `display-line-numbers-mode` in `prog-mode-hook`.
- [ ] Add **yasnippet** or **tempel** for snippet/template support.
