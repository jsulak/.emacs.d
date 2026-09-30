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
- Ansible, if using the included Debian/Ubuntu provisioning playbook
- Ansible Lint, Gitleaks, and Zizmor for the complete quality suite
- Git and a C compiler/toolchain
- Enchant and a spelling dictionary for Jinx
- `ripgrep` for project search
- Pandoc for Markdown/rich-text to Org conversion
- JetBrains Mono for the configured GUI font

The Debian/Ubuntu dependencies can be installed with
[`ansible/emacs.yml`](ansible/emacs.yml). Run it locally with:

```sh
ansible-playbook --ask-become-pass --inventory localhost, \
  --connection local ansible/emacs.yml
```

### macOS

This configuration is used with [Emacs for Mac OS X](https://emacsformacosx.com/).
Homebrew can install the external tools and font:

```sh
brew install ansible-lint enchant gitleaks pkg-config ripgrep pandoc zizmor
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
| `site-lisp/james-org-readable.el`, `bin/export-org-readable.sh` | Incremental PDFs for phone reading |
| `bin/backup-org.sh` | macOS Org archives, file mirror, and PDF publication |
| `site-lisp/james-markdown.el` | Markdown presentation and Zettelkasten helpers |
| `site-lisp/james-{linux,osx}.el` | Platform-specific setup |
| `site-lisp/james-{gui,tty}.el` | Display-specific setup |
| `site-lisp/james-theme.el`, `themes/` | Shared theme loader and Annex themes |
| `site-lisp/rich2org.el`, `bin/rich2org.sh` | macOS rich-text clipboard conversion |
| `ansible/emacs.yml` | Debian/Ubuntu system dependency provisioning |
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

For the checkbox lists in `todo-tasks.org`, run
`C-c A` in Org mode or `M-x james/org-archive-completed-checkboxes` from any
buffer. It moves completed
top-level items, including their notes and nested lists, into the existing
`Tasks Archive` heading in `archive.org`. Completed subtasks stay with unfinished
parents, and checked parents with unfinished children are skipped. The command
saves both files (including existing buffer edits), saving the archive first,
and refuses files that have changed on disk. If saving the source fails after
archiving, its removals remain in the buffer; save that buffer to finish.
Archiving is manual; no save hook or timer runs it.

### Capture and agenda

`C-c c` opens capture with these templates:

| Key | Template |
|-----|----------|
| `l` | Undated checkbox under `TODO Open Loops` in `todo-tasks.org` |
| `L` | Dated checkbox under `TODO Open Loops` in `todo-tasks.org` |
| `n` | New Org file with title, date, and tags |
| `o` | Unchecked 1:1 topic for a selected person |

Useful agenda commands:

| Key | View |
|-----|------|
| `C-c a w` | All `WAITING` items, ordered by deadline and schedule |
| `C-c a p` | Prompt for a person, then show their 1:1 topics, open loops, and tasks |
| `C-c p` | Open the person view directly |

The person view has separate sections for unchecked 1:1 topics, unchecked open
loops that mention the selected person, and matching Org tasks. Each 1:1 person
is represented by an Org file tagged `:person:` with a `Topics` heading.
`james/org-1on1-add-item` is also available as a non-interactive API for tools
that call Emacs through `emacsclient`.

### Images and attachments

Clipboard images, screenshots, URL downloads, and image drag-and-drop are
stored below the collection-wide `images/` directory. Other dragged or
explicitly attached files are copied below `attachments/` by default. In both
cases, the Org file's path relative to `org-directory` is mirrored without its
extension:

```text
~/org/meeting-notes.org  ->  ~/org/images/meeting-notes/
~/org/projects/alpha.org ->  ~/org/attachments/projects/alpha/
```

Set `james/org-attachment-root` in `local.el` to place non-image attachments
somewhere else. Relative values are resolved below `org-directory`; absolute
values can point outside the Org collection:

```elisp
(setq james/org-attachment-root "~/Documents/org-attachments")
```

With that setting, attachments for `~/org/projects/alpha.org` are stored in
`~/Documents/org-attachments/projects/alpha/`. Existing attachments are not
moved when the option changes; it affects only files added afterward.

Filenames use `YYYYMMDD-HHMMSS-sanitized-source-name.ext`; collisions receive
suffixes such as `-2` and `-3`. Links remain relative, ordinary `file:` links
that display the original filename and do not add Org ID property drawers.
Shared migrated attachments can live in `attachments/_shared/`, with
unreferenced files preserved in `attachments/_orphans/`.

The Org buffer must already be saved beneath `org-directory`. Renaming an Org
file does not move existing media, so old links remain valid while newly added
files use the new mirrored path. On macOS, non-Org attachments open in their
default application; Org links stay in Emacs.

New images inserted through drag-and-drop, clipboard paste, screenshots, or
`org-download` are recognized locally with Tesseract in the background. The
extracted text is stored directly below the image in an `:OCR:` drawer, folded
on insertion and when opening or reverting a note. Press `TAB` on the drawer
to inspect it. OCR drawers are excluded from Org exports, including rich-text
copy. Text lines use Org's fixed-width syntax to keep recognized characters
from becoming headings or drawer delimiters.

OCR text is cleaned before insertion: blank lines are removed, repeated spaces
and tabs become single spaces, and page breaks/line endings are normalized.
Nonempty line boundaries, bullet markers, and punctuation remain intact;
cleanup does not guess how columns or wrapped sentences should be joined.

`C-c r` (`consult-ripgrep`) searches the saved OCR text along with other note
content; search the Org collection directory to cover all notes. In-buffer
search can find it before saving. Native Org folding allows search navigation
to reveal matches. The drawer includes the image hash and recognition options
so unchanged images can be skipped on subsequent runs.

- `C-c v o` / `M-x james/org-ocr-image-at-point`: recognize the image link at
  point; a prefix argument forces a refresh of its existing transcription.
- `M-x james/org-ocr-buffer`: process missing or changed images in this buffer.
- `M-x james/org-ocr-cleanup-buffer`: tidy existing generated OCR drawers without
  rerunning recognition. Keeps metadata and other note content, supports undo,
  and leaves changes for normal saving. Unrecognized drawer formats are skipped.
- `M-x james/org-ocr-directory`: recursively backfill a selected local directory
  of Org files, reusing visiting buffers and leaving newly opened buffers open.
- `*Org OCR log*`: completion, skip, and failure messages.

OCR runs one image at a time, with a two-minute timeout per image. Results are
inserted into the live buffer, preserving point and unrelated edits, and left
for ordinary save/autosave. No OCR process writes an Org file. If the link,
existing drawer, or image changes during recognition, or the buffer closes,
the result is skipped. Backfill likewise preserves unsaved edits in visiting
buffers. It does not force saves; the existing Org autosave mode still applies.

Tesseract must be on Emacs's `exec-path` (`brew install tesseract` on macOS;
the Ansible playbook installs it on Debian/Ubuntu). Customize
`james/org-ocr-enabled` to disable automatic recognition, or
`james/org-ocr-arguments` to change language/layout. The default is English
sparse-text mode (`--psm 11`); `--psm 3` is an alternative for dense layouts.
Recognition quality varies with resolution and slide design. Unsupported
image formats (such as SVG or HEIC) produce a diagnostic; convert them to PNG
before recognition. Image insertion remains usable if recognition fails.

### Backup and reading copies

The macOS `com.james.backup-org` LaunchAgent runs `~/bin/backup-org.sh` every
30 minutes. Its tracked source is `bin/backup-org.sh`; install updates with
`install -m 755 ~/.emacs.d/bin/backup-org.sh ~/bin/backup-org.sh`.

Both shell launchers read the ignored `~/.emacs.d/local-org.env`. Keep the
machine's Org source and backup destination there, rather than in tracked code:

```sh
export ORG_SOURCE_DIR="${ORG_SOURCE_DIR:-${HOME}/org}"
export ORG_BACKUP_DESTINATION="${ORG_BACKUP_DESTINATION:-/path/to/backup/storage}"
```

The parameter expansions preserve explicit environment overrides. Set
`ORG_BACKUP_CONFIG` to load a different local configuration file. Both paths
are required; missing settings stop the scripts before any backup work.
`local.el` remains the place for interactive Emacs settings, including
`org-directory` and attachment paths.

Inside `$ORG_BACKUP_DESTINATION/org-backups/`:

- `org-latest.zip` and `snapshots/` contain the existing archive backups.
- `current/` mirrors the active Org tree, including images and attachments.
  Deletions flow from Org to this folder; phone edits do not flow back.
- `readable/` contains one PDF per `.org` file, using matching relative names.

PDFs use 12-point TeX Gyre Heros, A6 portrait pages, unnumbered heading
bookmarks, and clickable external links. PNG/JPEG images are embedded without
resampling and scaled to fit the page. Tables wrap within columns; tables with
four or more columns use landscape pages. Lists deeper than three levels
are flattened while retaining every item. Flags and document emoji become
printable labels. Other characters unavailable in the PDF font fall back to
visible Unicode-name labels such as `[SNOWMAN]`, with a message in the export
log. TODO states, planning dates, lists, checkboxes, and emphasis
are preserved; drawers, property metadata, and OCR text are omitted.
Local file links retain descriptions and paths rather than generating links
that OneDrive's mobile PDF viewer cannot reliably open.

The standalone exporter loads only built-in Emacs libraries, does not execute
Babel blocks, and does not modify Org files. Content hashes track the note,
linked PNG/JPEG files, and the exporter template. Each PDF is compiled in a
temporary directory, then replaced atomically. A failed file keeps its previous
PDF and is retried next time; other files continue. A full run removes only
previously generated PDFs whose sources were deleted. Compilation logs and
the successful-export manifest live under `org-backups/.state/readable/`.
The existing launchd stdout/stderr logs contain per-file failures and totals.
Archive work finishes before the mirror and PDF steps run, including on runs
where the archive fingerprint is unchanged.

Run all exports manually, or name specific files for a partial run:

```sh
~/.emacs.d/bin/export-org-readable.sh
~/.emacs.d/bin/export-org-readable.sh 'meeting-notes.org'
```

`ORG_SOURCE_DIR` and `ORG_BACKUP_DESTINATION` configure the source collection
and parent of `org-backups/`. `ORG_READABLE_EMACS` overrides the Emacs executable;
`ORG_READABLE_SCRIPT` overrides the exporter called by the backup script.
The launcher adds `/Library/TeX/texbin` and Homebrew to its PATH for launchd.
XeLaTeX and TeX Gyre fonts are required; the Ansible playbook supplies them on
Debian/Ubuntu. macOS provisioning is documented in `ANSIBLE-TODO.md`.

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

This runs full-history secret detection, GitHub Actions security analysis,
Ansible safety linting, hermetic ERT tests, a full startup smoke test, strict
byte compilation, built-in static checks, and shell tests for clipboard conversion and the
macOS backup/publication pipeline.
Individual targets are available as `make security`, `make ansible`,
`make test`, `make smoke`, `make byte-compile`, `make static`, and `make shell`.
Override the executable when needed, for example
`make check EMACS=/path/to/emacs`.
