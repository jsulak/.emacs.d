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

- [ ] `find-tag` is obsolete (`james-bindings.el:22`). Emacs 25+ uses `xref-find-definitions`. The `etags` require and `my-ido-find-tag` are also outdated.
- [ ] Smex is unmaintained (last update 2015). Consider `amx` (drop-in fork) or the vertico + consult + orderless + marginalia stack.
- [ ] `uniquify-seperator` is a typo (`init.el:180`) — should be `uniquify-separator`.
- [ ] `pending-delete-mode` (`init.el:208`) is redundant with `delete-selection-mode` (line 124).
- [ ] Clean up unused files in `external/`: `smart-tab.el`, `color-theme-blue.el`, `xquery-mode.el`, `jshint-mode` submodule, `w32-browser.el` (Windows-only).
- [ ] `fset 'yes-or-no-p` hack (`init.el:130`) — Emacs 28+ supports `(setopt use-short-answers t)`.

## Low Priority

- [ ] `python -mjson.tool` in `pretty-print-json` — consider `python3` or `jq`.
- [ ] Adopt `use-package` (built into Emacs 29) for cleaner config management.
- [ ] Consider replacing ido with vertico + consult + orderless + marginalia.
