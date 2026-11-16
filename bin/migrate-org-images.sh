#!/bin/bash
# Plan, apply, or verify reviewed OCR-based Org image renames.
set -euo pipefail

if [[ $# -ne 2 ]] || [[ ! "$1" =~ ^(plan|apply|verify)$ ]]; then
  printf 'Usage: %s plan|apply|verify MANIFEST\n' "$0" >&2
  exit 2
fi

config_file="${ORG_BACKUP_CONFIG:-${HOME}/.emacs.d/local-org.env}"
if [[ -f "$config_file" ]]; then
  # shellcheck source=/dev/null
  source "$config_file"
fi
: "${ORG_SOURCE_DIR:?Set ORG_SOURCE_DIR in local-org.env or the environment}"
: "${ORG_BACKUP_DESTINATION:?Set ORG_BACKUP_DESTINATION in local-org.env or the environment}"
export ORG_SOURCE_DIR ORG_BACKUP_DESTINATION

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
manifest=$2

if [[ $1 == apply ]]; then
  "$script_dir/backup-org.sh"
  backup_archive="${ORG_BACKUP_DESTINATION}/org-backups/org-latest.zip"
  /usr/bin/zip -T "$backup_archive" >/dev/null
  export ORG_IMAGE_MIGRATION_BACKUP_ARCHIVE="$backup_archive"
fi

export PATH="/opt/homebrew/bin:/usr/local/bin:${PATH}"
exec emacs --batch -Q \
  -L "$script_dir/../site-lisp" \
  -l james-org-image-migrate \
  -f james/org-image-migrate-batch -- "$1" "$manifest"
