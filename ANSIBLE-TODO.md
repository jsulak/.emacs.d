# Ansible TODO

There are no outstanding Debian/Ubuntu runtime provisioning changes.

The Emacs runtime, build, spelling, search, conversion, linting, and terminal
terminfo dependencies previously listed here are now provisioned directly by
[`ansible/emacs.yml`](ansible/emacs.yml).

Terminal applications must still advertise their actual color capabilities
through the appropriate `TERM` value. The playbook installs `ncurses-term`, but
does not set a global `TERM` value because it must match the launching terminal.

## macOS quality tooling

The complete quality suite now requires Ansible Lint, Gitleaks, and Zizmor.
They are installed locally with Homebrew. Add the following task when macOS
workstation provisioning is brought under Ansible:

```yaml
- name: Install Emacs quality and security tools
  community.general.homebrew:
    name:
      - ansible-lint
      - gitleaks
      - zizmor
    state: present
```

## macOS slide OCR

Slide OCR requires Tesseract with English language data. It is installed on
this machine; provision it on other Macs with `brew install tesseract` and
ensure the Homebrew bin directory is on Emacs's `exec-path`. Add this task
when macOS workstation provisioning is brought under Ansible:

```yaml
- name: Install local slide text recognition
  community.general.homebrew:
    name: tesseract
    state: present
```

For other OCR languages, install `tesseract-lang` and customize
`james/org-ocr-arguments` (for example, `("-l" "eng+fra" "--psm" "11")`).

## macOS Org reading PDFs

The batch PDF exporter uses the installed TeX Live/BasicTeX distribution at
`/Library/TeX/texbin`, including XeLaTeX, fontspec, geometry, graphicx, longtable,
array, booktabs, amsmath, amssymb, ulem, xcolor, hyperref, pdflscape, and the TeX
Gyre Heros/Cursor OpenType fonts. These dependencies are present on this Mac.
For future macOS provisioning, install MacTeX's complete package set:

```sh
brew install --cask mactex-no-gui
install -m 755 ~/.emacs.d/bin/backup-org.sh ~/bin/backup-org.sh
```

The existing `~/Library/LaunchAgents/com.james.backup-org.plist` invokes that
script every 1800 seconds. Its readable exporter is invoked from
`~/.emacs.d/bin/export-org-readable.sh`; no interactive Emacs server is required.
The LaunchAgent installation itself is still machine-local provisioning.
Provision the ignored `~/.emacs.d/local-org.env` with `ORG_SOURCE_DIR` and
`ORG_BACKUP_DESTINATION` for each machine (see README.md). Install the updated
backup script with the command above so scheduled runs load that configuration.
