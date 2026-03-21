# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Emacs configuration targeting Emacs 29+. Uses `use-package` (built-in) with `package.el` for package management. Packages auto-install from MELPA/MELPA-stable via `use-package-always-ensure t`.

## Architecture

- **`init.el`** — Main entry point. All package declarations and keybindings live here.
- **`site-lisp/james-functions.el`** — Custom elisp functions (~25 utilities for editing, navigation, window management, text formatting).
- **`site-lisp/james-{linux,osx,windows}.el`** — Platform-specific configuration, loaded conditionally.
- **`site-lisp/james-gui.el`** — GUI-only settings (loaded when `display-graphic-p`).
- **`custom.el`** — Emacs customize output (separate from init.el).
- **`local.el`** — Optional machine-local overrides (not tracked in git).
## Key Conventions

- **Keybindings** are centralized using `use-package :bind` and `bind-keys` blocks in `init.el`. Don't scatter bindings across files.
- **`C-h` is remapped to backspace**; help is on `F1`. This is a core ergonomic choice.
- Custom functions use advice (`slick-copy-advice`, `slick-cut-advice`) to extend kill/copy to work on whole lines when no region is active.
- Platform detection uses `system-type` checks in init.el to load the appropriate `james-*.el` file.

## Modernization Roadmap

See `AUDIT.md` for the planned modernization path (vertico/consult stack, magit, eglot, corfu, etc.). When making changes, prefer modern Emacs 29+ idioms and built-in packages where possible.
