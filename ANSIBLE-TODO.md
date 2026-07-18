# Ansible TODO

System-level dependencies for this Emacs configuration that should be provisioned via Ansible.

## Packages

```yaml
- name: Install Emacs dependencies
  package:
    name:
      - enchant-2
      - libenchant-2-dev
      - hunspell
      - hunspell-en-us
      - build-essential
      - pkg-config
      - ripgrep
      - pandoc
      - python3
      - git
      - shellcheck
    state: present
```

## Terminal truecolor

The annex theme relies on 24-bit color. For `emacs -nw` to render it
correctly, the terminal must advertise truecolor via terminfo. Ensure
`ncurses-term` is installed (provides `xterm-direct` etc.) and set
`TERM=xterm-direct` (or `tmux-direct`, `alacritty-direct`, etc. matching
the terminal) in the shell environment used to launch Emacs.

```yaml
- name: Install extended terminfo entries
  package:
    name:
      - ncurses-term
    state: present
```
