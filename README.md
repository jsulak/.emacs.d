# My Emacs settings

Personal Emacs configuration targeting Emacs 30+. Uses `use-package` (built-in) with `package.el` for package management. Packages auto-install from MELPA via `use-package-always-ensure t`.

## Setup

    git clone git://github.com/jsulak/.emacs.d.git

The first time you start Emacs, it will install some additional packages
that are best handled by the package manager.

### Install Emacs on macOS

I use [Emacs For Mac OS X](http://emacsformacosx.com).

### Font

Install JetBrains Mono:

    brew install --cask font-jetbrains-mono

## Architecture

- **`init.el`** -- Main entry point. All package declarations and keybindings live here.
- **`site-lisp/james-functions.el`** -- Custom elisp functions (editing, navigation, window management, text formatting).
- **`site-lisp/james-osx.el`** -- macOS-specific configuration.
- **`site-lisp/james-linux.el`** -- Linux-specific configuration.
- **`site-lisp/james-gui.el`** -- GUI-only settings (theme, font).
- **`custom.el`** -- Emacs customize output (separate from init.el).
- **`local.el`** -- Optional machine-local overrides (not tracked in git).

## Key packages

| Package | Purpose |
|---------|---------|
| vertico + orderless + marginalia + consult | Completion framework |
| corfu | In-buffer completion |
| eglot | LSP client (Python, JS, TS, CSS) |
| magit | Git interface |
| diff-hl | Git change indicators in fringe |
| treemacs | File tree sidebar |
| eat | Terminal emulator |
| org-download | Image paste/drag-and-drop in org |
| expand-region | Smart selection expansion |
| mood-line | Clean mode line |

## Key bindings

`C-h` is rebound to backspace; help is on `F1`.

### Files

| Key | Command |
|-----|---------|
| `C-x C-f` | Open a file |
| `C-x C-s` | Save file |
| `C-x b` | Switch buffer (consult) |
| `C-x C-r` | Recent files |
| `C-c j` | Dired jump |
| `F5` | Revert buffer |

### Editing

| Key | Command |
|-----|---------|
| `C-x C-k` | Kill region |
| `C-w` | Kill word forward |
| `C-q` | Kill word backward |
| `M-w` | Copy (whole line if no selection) |
| `C-y` | Yank |
| `M-y` | Yank from kill ring (consult) |
| `C-z` | Undo |
| `C-x u` | Visual undo tree (vundo) |
| `C-'` | Expand region |
| `C-c y` | Duplicate line |
| `C-;` | Toggle comment |
| `C-S-up/down` | Move line up/down |
| `C-return` | Open line below |
| `C-S-return` | Open line above |

### Navigation

| Key | Command |
|-----|---------|
| `C-a` | Smart beginning of line |
| `M-.` | Go to definition (xref) |
| `C-.` | Go back (xref) |
| `C-c g` | Go to line |
| `C-c i` | Imenu (consult) |
| `C-c o` | Search lines in buffer (consult) |
| `C-c r` | Ripgrep (consult) |
| `C-x C-d` | Switch directory (consult-dir) |

### Windows

| Key | Command |
|-----|---------|
| `S-arrow` | Move between windows |
| `C-x 5` | Split and show next buffer |
| `F6` | Swap two windows |
| `F9` | Toggle treemacs |

### Org mode

| Key | Command |
|-----|---------|
| `C-c a` | Org agenda |
| `C-c c` | Org capture |
| `C-c l` | Org store link |

### Other

| Key | Command |
|-----|---------|
| `C-x g` | Magit status |
| `C-c e` | Eat terminal |
| `C-x t e` | Eat terminal (project) |
| `C-c C` | Claude Code prefix |
| `M-;` | Hippie expand |
| `C-+` / `C--` | Increase / decrease font size |
| `M-x` or `C-x C-m` | Execute command |

## Tips

- I recommend rebinding Caps Lock to Ctrl.
- Machine-specific settings go in `local.el`.
