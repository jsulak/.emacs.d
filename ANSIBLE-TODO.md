# Ansible TODO

There are no outstanding Debian/Ubuntu provisioning changes.

The Emacs runtime, build, spelling, search, conversion, linting, and terminal
terminfo dependencies previously listed here are now provisioned directly by
[`ansible/emacs.yml`](ansible/emacs.yml).

Terminal applications must still advertise their actual color capabilities
through the appropriate `TERM` value. The playbook installs `ncurses-term`, but
does not set a global `TERM` value because it must match the launching terminal.
