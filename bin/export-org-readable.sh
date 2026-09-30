#!/bin/bash
# Export all Org notes, or the relative .org filenames given as arguments.
# ORG_SOURCE_DIR and ORG_BACKUP_DESTINATION match backup-org.sh.
set -euo pipefail

config_file="${ORG_BACKUP_CONFIG:-${HOME}/.emacs.d/local-org.env}"
if [[ -f "$config_file" ]]; then
  # shellcheck source=/dev/null
  source "$config_file"
fi
: "${ORG_SOURCE_DIR:?Set ORG_SOURCE_DIR in local-org.env or the environment}"
: "${ORG_BACKUP_DESTINATION:?Set ORG_BACKUP_DESTINATION in local-org.env or the environment}"
export ORG_SOURCE_DIR ORG_BACKUP_DESTINATION

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
export PATH="/Library/TeX/texbin:/opt/homebrew/bin:/usr/local/bin:${PATH}"
emacs_binary="${ORG_READABLE_EMACS:-emacs}"

exec "$emacs_binary" --batch -Q \
  -l "$script_dir/../site-lisp/james-org-readable.el" \
  -f james/org-readable-batch -- "$@"
