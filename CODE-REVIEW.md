# Code Review

## Findings

1. **High: Rename/move can hide unsaved edits and cause data loss.**
   `site-lisp/james-functions.el:52-66`, `site-lisp/james-functions.el:69-83`

   Both commands call `set-buffer-modified-p nil` after moving the on-disk
   file. If the buffer has unsaved edits, its contents differ from disk but
   Emacs no longer warns before closing it. This was reproduced with
   `modified=nil`, buffer contents of `"unsaved-edit"`, and disk contents of
   `"disk-old"`.

2. **High: Reloading the configuration leaves startup optimizations permanently enabled.**
   `init.el:13-20`, `site-lisp/james-functions.el:111-120`

   Reloading sets `file-name-handler-alist` to `nil` and the GC threshold to
   `most-positive-fixnum`, but restoration only runs from
   `emacs-startup-hook`, which has already fired. This can break TRAMP and
   compressed-file handling and allow excessive memory growth.

3. **High: Checkbox archiving accepts fake headings inside Org blocks.**
   `site-lisp/james-org.el:1020-1030`

   Destination discovery searches with `org-heading-regexp` without
   confirming that the match is an actual Org headline. A sole
   `* Tasks Archive` inside an example or source block is accepted; the
   archive is saved and the source items are deleted. This behavior was
   reproduced.

4. **High: 1:1 person names can escape `org-directory`.**
   `site-lisp/james-org.el:1127-1133`, `site-lisp/james-org.el:1279-1297`

   A name such as `../outside` resolves outside the Org collection. The
   documented noninteractive `james/org-1on1-add-item` API can therefore
   create or modify arbitrary `.org` files reachable by path.

5. **Medium: Invalid JSON formatting destroys the original text.**
   `site-lisp/james-functions.el:214-219`

   `shell-command-on-region` replaces the source regardless of exit status.
   Invalid JSON was reproduced as being replaced by Python's
   `Expecting value...` error instead of preserving the input.

6. **Medium: Backup change detection can leave `org-latest.zip` stale.**
   `bin/backup-org.sh:284-300`

   The fingerprint includes names, sizes, modes, and modification times, but
   not contents. Content replaced with equal-length data and a preserved
   timestamp is treated as unchanged indefinitely.

7. **Medium: ZIP backups may include files outside the Org tree through symlinks.**
   `bin/backup-org.sh:305-310`

   The ZIP invocation follows symlinks by default. A symlink below `~/org`
   can copy external content, potentially including sensitive files, into the
   cloud backup.

8. **Low: The recent blank-line fix misses an unterminated whitespace-only final line.**
   `site-lisp/james-org.el:1032-1045`

   At EOF containing spaces but no newline, the cleanup checks the preceding
   line rather than the current whitespace-only line. Archiving retains that
   apparent blank line. The test at
   `test/james-org-archive-test.el:70-75` only covers newline-terminated blank
   lines.

## Verification

- All 91 ERT tests passed.
- Static checks passed.
- Strict byte compilation passed.
- The worktree was clean at the time of review.
