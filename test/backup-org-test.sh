#!/bin/bash
# Integration checks for the macOS launchd backup/publication pipeline.
set -euo pipefail
if [[ $(uname -s) != Darwin ]]; then
  echo 'Skipping macOS backup integration checks on this platform'
  exit 0
fi

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
export ORG_SOURCE_DIR="$work_dir/active org"
export ORG_BACKUP_DESTINATION="$work_dir/OneDrive folder"
export ORG_BACKUP_CONFIG="$work_dir/local-org.env"
cat > "$ORG_BACKUP_CONFIG" <<CONFIG
export ORG_SOURCE_DIR="\${ORG_SOURCE_DIR:-$ORG_SOURCE_DIR}"
export ORG_BACKUP_DESTINATION="\${ORG_BACKUP_DESTINATION:-$ORG_BACKUP_DESTINATION}"
CONFIG
# The first backup must obtain both paths from the local file.
export -n ORG_SOURCE_DIR ORG_BACKUP_DESTINATION
export ORG_READABLE_SCRIPT="$work_dir/exporter.sh"
export ORG_TEST_EXPORT_LOG="$work_dir/exporter.log"
export ORG_TEST_EXPORT_FAIL="$work_dir/fail"
backup_dir="$ORG_BACKUP_DESTINATION/org-backups"
mkdir -p "$ORG_SOURCE_DIR/images"
printf '* First note\n' > "$ORG_SOURCE_DIR/note.org"
printf 'image\n' > "$ORG_SOURCE_DIR/images/example.png"
printf 'autosave\n' > "$ORG_SOURCE_DIR/#note.org#"
cat > "$ORG_READABLE_SCRIPT" <<'STUB'
#!/bin/bash
set -euo pipefail
printf 'export\n' >> "$ORG_TEST_EXPORT_LOG"
[[ ! -e "$ORG_TEST_EXPORT_FAIL" ]]
STUB

/bin/bash "$repo_dir/bin/backup-org.sh" > "$work_dir/output" 2>&1
cmp "$ORG_SOURCE_DIR/note.org" "$backup_dir/current/note.org"
cmp "$ORG_SOURCE_DIR/images/example.png" "$backup_dir/current/images/example.png"
[[ ! -e "$backup_dir/current/#note.org#" ]]
[[ $(wc -l < "$ORG_TEST_EXPORT_LOG") -eq 1 ]]
cp "$backup_dir/org-latest.zip" "$work_dir/first.zip"

# Explicit environment paths must override the local file's defaults.
export ORG_SOURCE_DIR ORG_BACKUP_DESTINATION
cat > "$ORG_BACKUP_CONFIG" <<CONFIG
export ORG_SOURCE_DIR="\${ORG_SOURCE_DIR:-$work_dir/unused-source}"
export ORG_BACKUP_DESTINATION="\${ORG_BACKUP_DESTINATION:-$work_dir/unused-destination}"
CONFIG

# The standalone PDF launcher also passes local paths to batch Emacs.
cat > "$work_dir/emacs-stub" <<'STUB'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$ORG_SOURCE_DIR" "$ORG_BACKUP_DESTINATION"
STUB
chmod +x "$work_dir/emacs-stub"
ORG_READABLE_EMACS="$work_dir/emacs-stub" \
  /bin/bash "$repo_dir/bin/export-org-readable.sh" > "$work_dir/paths"
printf '%s\n' "$ORG_SOURCE_DIR" "$ORG_BACKUP_DESTINATION" > "$work_dir/expected-paths"
cmp "$work_dir/expected-paths" "$work_dir/paths"
(
  unset ORG_SOURCE_DIR ORG_BACKUP_DESTINATION
  ORG_READABLE_EMACS="$work_dir/emacs-stub" \
    /bin/bash "$repo_dir/bin/export-org-readable.sh" > "$work_dir/paths"
)
printf '%s\n' "$work_dir/unused-source" "$work_dir/unused-destination" > "$work_dir/expected-paths"
cmp "$work_dir/expected-paths" "$work_dir/paths"

# Missing configuration fails before creating any backup directories.
for launcher in backup-org.sh export-org-readable.sh; do
  if (
    unset ORG_SOURCE_DIR ORG_BACKUP_DESTINATION
    ORG_BACKUP_CONFIG="$work_dir/missing.env" \
      /bin/bash "$repo_dir/bin/$launcher" > "$work_dir/missing-output" 2>&1
  ); then
    echo "Expected missing configuration failure: $launcher" >&2
    exit 1
  fi
done
[[ ! -e "$work_dir/unused-destination" ]]

# Unchanged archives still repair the mirror and invoke the exporter.
rm "$backup_dir/current/note.org"
printf 'stale\n' > "$backup_dir/current/stale.org"
/bin/bash "$repo_dir/bin/backup-org.sh" >> "$work_dir/output" 2>&1
cmp "$ORG_SOURCE_DIR/note.org" "$backup_dir/current/note.org"
[[ ! -e "$backup_dir/current/stale.org" ]]
cmp "$work_dir/first.zip" "$backup_dir/org-latest.zip"
[[ $(wc -l < "$ORG_TEST_EXPORT_LOG") -eq 2 ]]

# A mirror failure still runs PDF publication after updating the archive.
rm -rf "$backup_dir/current"
printf 'blocked\n' > "$backup_dir/current"
printf '* New note\n' > "$ORG_SOURCE_DIR/new.org"
if /bin/bash "$repo_dir/bin/backup-org.sh" >> "$work_dir/output" 2>&1; then
  echo 'Expected mirror failure' >&2
  exit 1
fi
[[ $(wc -l < "$ORG_TEST_EXPORT_LOG") -eq 3 ]]
unzip -p "$backup_dir/org-latest.zip" 'active org/new.org' > "$work_dir/archived.org"
cmp "$ORG_SOURCE_DIR/new.org" "$work_dir/archived.org"
[[ ! -e "$backup_dir/.state/lock" ]]

# A PDF failure is reported after the mirror succeeds; next run retries it.
rm "$backup_dir/current"
touch "$ORG_TEST_EXPORT_FAIL"
if /bin/bash "$repo_dir/bin/backup-org.sh" >> "$work_dir/output" 2>&1; then
  echo 'Expected PDF export failure' >&2
  exit 1
fi
cmp "$ORG_SOURCE_DIR/new.org" "$backup_dir/current/new.org"
[[ ! -e "$backup_dir/.state/lock" ]]
rm "$ORG_TEST_EXPORT_FAIL"
/bin/bash "$repo_dir/bin/backup-org.sh" >> "$work_dir/output" 2>&1
[[ $(wc -l < "$ORG_TEST_EXPORT_LOG") -eq 5 ]]
echo 'macOS backup and publication integration checks passed'
