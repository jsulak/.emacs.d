# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Emacs configuration targeting Emacs 30+. Uses `use-package` (built-in) with `package.el` for package management. Packages auto-install from MELPA via `use-package-always-ensure t`.

## Architecture

- **`init.el`** — Main entry point. All package declarations and keybindings live here.
- **`site-lisp/james-functions.el`** — Custom elisp functions (~25 utilities for editing, navigation, window management, text formatting).
- **`site-lisp/james-{linux,osx}.el`** — Platform-specific configuration, loaded conditionally.
- **`site-lisp/james-gui.el`** — GUI-only settings (loaded when `display-graphic-p`).
- **`custom.el`** — Emacs customize output (separate from init.el).
- **`local.el`** — Optional machine-local overrides (not tracked in git).
## Key Conventions

- **Keybindings** are centralized using `use-package :bind` and `bind-keys` blocks in `init.el`. Don't scatter bindings across files.
- **`C-h` is remapped to backspace**; help is on `F1`. This is a core ergonomic choice.
- Custom functions use advice (`slick-copy-advice`, `slick-cut-advice`) to extend kill/copy to work on whole lines when no region is active.
- Platform detection uses `system-type` checks in init.el to load the appropriate `james-*.el` file.

## Org Mode

- **`site-lisp/james-org.el`** — All org-mode configuration lives here (not in `init.el`).
- **TODO keywords**: `TODO(t)` → `WAITING(w)` → `ON HOLD(h)` | `DONE(d)` → `OBE(c)`
- **Capture templates** (`C-c c`):
  - `l` / `L` — Open loop (scheduled / deadline), stored in `open-loops.org`
  - `n` — New org file with title, date, and tags
  - `o` — 1:1 agenda item (checkbox under a person's heading in `agendas.org`)
- **Custom agenda views** (`C-c a`):
  - `w` — All WAITING items
  - `p` — Person view: 1:1 checklist items + TODOs/WAITING mentioning that person
- **1:1 agenda system** (`C-c p`):
  - `agendas.org` holds per-person checklists with one top-level heading per person
  - `james/org-agenda-person-view` prompts for a name (partial match supported) and shows their checklist + related tasks
  - `james/org-1on1-add-item` provides a non-interactive API for external tools (e.g. Alfred via emacsclient)

## System-Level Changes

When changes require system-level setup (apt packages, npm global/local installs, PATH modifications, etc.), document them in `ANSIBLE-TODO.md` with the corresponding Ansible YAML snippets so the provisioning playbook can be updated.

When making changes, prefer modern Emacs 30+ idioms and built-in packages where possible.
