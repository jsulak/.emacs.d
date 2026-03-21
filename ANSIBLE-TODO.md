# Ansible TODO: TypeScript/Tree-sitter Support for Emacs

Instructions for updating the Ansible playbook that provisions this machine's Emacs environment.

## 1. Install `gcc` (apt package)

Tree-sitter grammars must be compiled from C source. Emacs's `treesit-auto` package handles downloading and compiling them automatically, but it needs a C compiler.

Add `gcc` to the list of apt packages:

```yaml
- name: Install gcc for tree-sitter grammar compilation
  apt:
    name: gcc
    state: present
```

## 2. Install TypeScript language server (npm)

Eglot needs `typescript-language-server` to provide LSP features (completion, diagnostics, go-to-definition) in TypeScript/TSX files. Install it along with `typescript` itself.

Since global npm install requires root, install to `~/.local` for the target user:

```yaml
- name: Install TypeScript language server
  npm:
    name: "{{ item }}"
    path: "~/.local"
  loop:
    - typescript
    - typescript-language-server
  become: false
```

This places binaries in `~/.local/node_modules/.bin/`.

## 3. Add node_modules bin to PATH (bashrc)

Ensure `~/.local/node_modules/.bin` is on the user's PATH so Emacs (via `exec-path-from-shell`) and shell sessions can find `typescript-language-server`.

Add this line to `~/.bashrc`:

```bash
export PATH="$HOME/.local/node_modules/.bin:$PATH"
```

In Ansible:

```yaml
- name: Add local node_modules bin to PATH
  lineinfile:
    path: "~/.bashrc"
    line: 'export PATH="$HOME/.local/node_modules/.bin:$PATH"'
    state: present
  become: false
```

## Summary

After these three changes, opening a `.ts` or `.tsx` file in Emacs will:

1. Trigger `treesit-auto` to prompt for grammar installation (one-time, compiles with gcc)
2. Activate `typescript-ts-mode` or `tsx-ts-mode` with full syntax highlighting
3. Start `typescript-language-server` via eglot for LSP support
