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
