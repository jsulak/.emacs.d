#!/bin/bash
# Export all Org notes, or the relative .org filenames given as arguments.
# ORG_SOURCE_DIR and ORG_BACKUP_DESTINATION match backup-org.sh.
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
export PATH="/Library/TeX/texbin:/opt/homebrew/bin:/usr/local/bin:${PATH}"
emacs_binary="${ORG_READABLE_EMACS:-emacs}"

exec "$emacs_binary" --batch -Q \
  -l "$script_dir/../site-lisp/james-org-readable.el" \
  -f james/org-readable-batch -- "$@"
