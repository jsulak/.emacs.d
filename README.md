# Emacs configuration

[![Check](https://github.com/jsulak/.emacs.d/actions/workflows/check.yml/badge.svg)](https://github.com/jsulak/.emacs.d/actions/workflows/check.yml)

An opinionated personal Emacs configuration for Emacs 30 and newer. It uses
built-in `use-package` with `package.el`, installs third-party packages from
MELPA on first launch, and supports macOS and Linux in both graphical and
terminal sessions.

## Highlights

- Vertico, Orderless, Marginalia, Consult, and Corfu for completion and search
- Eglot and Tree-sitter modes for Python, JavaScript, TypeScript, TSX, and CSS
- Magit, Diff-HL, Treemacs, Eat, Avy, Jinx, and Vundo
- A focused Org workflow for capture, open loops, 1:1s, rich-text conversion,
  images, and attachments
- Markdown presentation and Zettelkasten helpers with wiki links and backlinks
- Matching custom light and dark themes, including truecolor terminal support
- Automated ERT, startup, byte-compilation, static, and shell checks in CI

## Requirements

- Emacs 30+
- Git and a C compiler/toolchain
- Enchant and a spelling dictionary for Jinx
- `ripgrep` for project search
- Pandoc for Markdown/rich-text to Org conversion
- JetBrains Mono for the configured GUI font

The Linux package list and provisioning snippets are maintained in
[`ANSIBLE-TODO.md`](ANSIBLE-TODO.md).

### macOS

This configuration is used with [Emacs for Mac OS X](https://emacsformacosx.com/).
Homebrew can install the external tools and font:

```sh
brew install enchant pkg-config ripgrep pandoc
brew install --cask font-jetbrains-mono
```

The rich-text clipboard converter is macOS-specific because it reads HTML from
the pasteboard with `osascript`.

### Terminal sessions

The Annex themes require a terminal advertising 24-bit color, normally through
a `*-direct` terminfo entry such as `xterm-direct` or `tmux-direct`. The `kkp`
package enables the Kitty keyboard protocol in compatible terminals so Emacs
can distinguish more modified key combinations.

## Install

Back up any existing Emacs configuration, then clone this repository into the
standard configuration directory:

```sh
git clone https://github.com/jsulak/.emacs.d.git ~/.emacs.d
emacs
```

The first launch refreshes the package archives and installs missing packages.
Tree-sitter grammars are offered for installation when a supported mode first
needs them.

Put machine-specific overrides in an untracked `local.el`, which loads last:

```elisp
(setq org-directory "~/org"
      org-agenda-files '("~/org"))

;; Optional Markdown/Zettelkasten location.
(setq james/zk-directory "~/notes/zettel")
```

Customize-generated settings are written to the untracked `custom.el` rather
than mixed into `init.el`.

## Repository layout

| Path | Purpose |
|------|---------|
| `early-init.el` | Pre-frame startup settings and UI flicker prevention |
| `init.el` | Package declarations, shared behavior, and centralized keybindings |
| `site-lisp/james-functions.el` | Editing, navigation, window, and conversion utilities |
| `site-lisp/james-org.el` | Org capture, agenda, 1:1, image, and attachment workflows |
| `site-lisp/james-markdown.el` | Markdown presentation and Zettelkasten helpers |
| `site-lisp/james-{linux,osx}.el` | Platform-specific setup |
| `site-lisp/james-{gui,tty}.el` | Display-specific setup |
| `site-lisp/james-theme.el`, `themes/` | Shared theme loader and Annex themes |
| `site-lisp/rich2org.el`, `bin/rich2org.sh` | macOS rich-text clipboard conversion |
| `test/`, `Makefile` | Local and CI quality checks |
| `local.el`, `custom.el` | Untracked local and Customize settings |

## Org workflow

Org defaults to `~/org`, with all files there included in the agenda. The task
sequence is:

```text
TODO -> WAITING -> ON HOLD | DONE -> OBE
```

`DONE` and `OBE` tasks can be archived together with
`M-x james/org-archive-done-tasks`.

### Capture and agenda

`C-c c` opens capture with these templates:

| Key | Template |
|-----|----------|
| `l` | Scheduled open loop in `open-loops.org` |
| `L` | Open loop with a deadline in `open-loops.org` |
| `n` | New Org file with title, date, and tags |
| `o` | Unchecked 1:1 topic for a selected person |

Useful agenda commands:

| Key | View |
|-----|------|
| `C-c a w` | All `WAITING` items, ordered by deadline and schedule |
| `C-c a p` | Prompt for a person, then show their 1:1 topics and related tasks |
| `C-c p` | Open the person view directly |

Each 1:1 person is represented by an Org file tagged `:person:` with a `Topics`
heading. `james/org-1on1-add-item` is also available as a non-interactive API
for tools that call Emacs through `emacsclient`.

### Images and attachments

Clipboard images, screenshots, URL downloads, and image drag-and-drop are
stored below the collection-wide `images/` directory. Other dragged or
explicitly attached files are copied below `attachments/`. In both cases, the
Org file's path relative to `org-directory` is mirrored without its extension:

```text
~/org/meeting-notes.org  ->  ~/org/images/meeting-notes/
~/org/projects/alpha.org ->  ~/org/attachments/projects/alpha/
```

Filenames use `YYYYMMDD-HHMMSS-sanitized-source-name.ext`; collisions receive
suffixes such as `-2` and `-3`. Links remain relative, ordinary `file:` links
and do not add Org ID property drawers. Shared migrated attachments can live in
`attachments/_shared/`, with unreferenced files preserved in
`attachments/_orphans/`.

The Org buffer must already be saved beneath `org-directory`. Renaming an Org
file does not move existing media, so old links remain valid while newly added
files use the new mirrored path. On macOS, non-Org attachments open in their
default application; Org links stay in Emacs.

## Keybindings

`C-h` is Backspace in this configuration; Emacs help remains available on
`F1`. Caps Lock remapped to Control is recommended.

### Files and navigation

| Key | Action |
|-----|--------|
| `C-x b` | Switch buffer with Consult |
| `C-x C-r` | Open a recent file |
| `C-x C-d` | Switch directory with Consult Dir |
| `C-c j` | Jump to the current file in Dired |
| `C-c i` | Navigate buffer symbols with Imenu |
| `C-c o` | Search lines in the current buffer |
| `C-c r` | Search a project with ripgrep |
| `M-j` | Jump with Avy |
| `M-.` / `C-.` | Go to definition / go back |
| `F5` | Revert the current buffer |

### Editing

| Key | Action |
|-----|--------|
| `C-x C-k` | Kill region, or the whole line without a selection |
| `M-w` | Copy region, or the whole line without a selection |
| `C-w` / `C-q` | Kill word forward / backward |
| `M-y` | Choose a previous kill with Consult |
| `C-z` / `C-x u` | Undo / open Vundo |
| `C-'` / `C-"` | Expand / contract the region |
| `C-c y` | Duplicate line |
| `C-;` | Toggle comment on line or region |
| `C-S-up` / `C-S-down` | Move the current line or region |
| `C-return` / `C-S-return` | Open a line below / above |

### Windows and tools

| Key | Action |
|-----|--------|
| `S-arrow` | Move between windows |
| `C-x 5` | Toggle a split showing the next buffer |
| `F6` | Swap two windows |
| `F9` | Toggle Treemacs for the current project |
| `C-x g` | Open Magit status |
| `C-c e` / `C-x t e` | Open Eat / project Eat |
| `C-c C` | Claude Code command prefix |
| `C-+` / `C--` | Increase / decrease font size |
| `M-x` / `C-x C-m` | Execute a command |

### Writing

| Key | Action |
|-----|--------|
| `C-c a` / `C-c c` | Org agenda / capture |
| `C-c l` | Store an Org link |
| `C-c f` | Insert a link to another file in `org-directory` |
| `C-c v a` | Copy a file into the current Org attachment directory |
| `C-c v s` / `C-c v y` | Add a screenshot / clipboard image |
| `C-c w` | Copy the Org region as rich text |
| `C-c m` | Convert Markdown from the kill ring to Org |
| `C-c z b` / `C-c z l` | Find backlinks / insert a wiki link in Markdown |

## Tests

Run the same complete quality suite used by GitHub Actions:

```sh
make check
```

This runs hermetic ERT tests, a full startup smoke test, strict byte
compilation, built-in static checks, and shell tests for `rich2org.sh`.
Individual targets are available as `make test`, `make smoke`,
`make byte-compile`, `make static`, and `make shell`. Override the executable
when needed, for example `make check EMACS=/path/to/emacs`.
