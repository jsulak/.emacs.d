#!/bin/bash

set -euo pipefail

source_dir="${ORG_SOURCE_DIR:-${HOME}/org}"
destination_root="${ORG_BACKUP_DESTINATION:-${HOME}/OneDrive - Raytheon Technologies}"
backup_dir="${destination_root}/org-backups"
snapshot_dir="${backup_dir}/snapshots"
mirror_dir="${backup_dir}/current"
state_dir="${backup_dir}/.state"
latest_archive="${backup_dir}/org-latest.zip"
fingerprint_file="${state_dir}/latest"
lock_file="${state_dir}/lock"
now_epoch="${ORG_BACKUP_NOW:-$(date +%s)}"

temp_archive="${backup_dir}/.org-latest.$$.zip"
lock_acquired=false

cleanup() {
  rm -f "$temp_archive"
  if [[ "$lock_acquired" == true ]]; then
    rm -f "$lock_file"
  fi
}
trap cleanup EXIT HUP INT TERM

if [[ ! -d "$source_dir" ]]; then
  echo "Org directory does not exist: $source_dir" >&2
  exit 1
fi

mkdir -p "$state_dir" "$snapshot_dir"

# launchd should not overlap runs, but the lock also protects manual invocations.
if ! (set -o noclobber; printf '%s\n' "$$" > "$lock_file") 2>/dev/null; then
  lock_pid=$(sed -n '1p' "$lock_file" 2>/dev/null || true)
  if [[ "$lock_pid" =~ ^[0-9]+$ ]] && kill -0 "$lock_pid" 2>/dev/null; then
    exit 0
  fi

  rm -f "$lock_file"
  if ! (set -o noclobber; printf '%s\n' "$$" > "$lock_file") 2>/dev/null; then
    echo "Could not acquire backup lock: $lock_file" >&2
    exit 1
  fi
fi
lock_acquired=true

snapshot_target() {
  local epoch="$1"
  local source="$2"
  local target="${snapshot_dir}/org-${epoch}.zip"
  local source_hash

  if [[ ! -e "$target" ]] || cmp -s "$source" "$target"; then
    printf '%s\n' "$target"
    return
  fi

  source_hash=$(/usr/bin/shasum -a 256 "$source" | /usr/bin/awk '{print substr($1, 1, 12)}')
  printf '%s\n' "${snapshot_dir}/org-${epoch}-${source_hash}.zip"
}

# Consolidate archives made by the previous tiered scheme. Modification times
# record when those snapshots were created, and identical tier copies therefore
# converge on the same target.
migrate_legacy_snapshots() {
  local tier
  local file
  local files
  local epoch
  local target

  for tier in recent daily weekly monthly; do
    files=( "${backup_dir}/${tier}"/org-*.zip )
    if [[ ! -e "${files[0]}" ]]; then
      rmdir "${backup_dir}/${tier}" 2>/dev/null || true
      continue
    fi

    for file in "${files[@]}"; do
      epoch=$(/usr/bin/stat -f '%m' "$file")
      target=$(snapshot_target "$epoch" "$file")
      if [[ -e "$target" ]]; then
        if cmp -s "$file" "$target"; then
          rm -f "$file"
        else
          echo "Snapshot collision left in place: $file" >&2
        fi
      else
        mv "$file" "$target"
      fi
    done

    rmdir "${backup_dir}/${tier}" 2>/dev/null || true
  done
}

recent_key() {
  local epoch="$1"
  local hour
  local bucket_hour

  hour=$(date -r "$epoch" +%H)
  printf -v bucket_hour '%02d' "$((10#$hour / 4 * 4))"
  printf '%s-%s%s' \
    "$(date -r "$epoch" +%Y%m%d)" \
    "$bucket_hour" \
    "$(date -r "$epoch" +%z)"
}

daily_key() {
  date -r "$1" +%Y-%m-%d
}

weekly_key() {
  date -r "$1" +%G-W%V
}

monthly_key() {
  date -r "$1" +%Y-%m
}

key_is_in() {
  local key="$1"
  local keys="$2"
  [[ "$keys" == *"|${key}|"* ]]
}

# Retain a union of recovery points. A file selected by several time scales is
# stored only once:
#   - one per four-hour bucket for the previous 24 hours
#   - one per day for the previous five completed days
#   - one per week for the previous four completed ISO weeks
#   - one per month for the previous twelve completed months
prune_snapshots() {
  local files
  local file
  local base
  local epoch
  local index
  local offset
  local key
  local keep
  local recent_cutoff=$((now_epoch - 86400))
  local daily_windows="|"
  local weekly_windows="|"
  local monthly_windows="|"
  local seen_recent="|"
  local seen_daily="|"
  local seen_weekly="|"
  local seen_monthly="|"

  for ((offset = 1; offset <= 5; offset++)); do
    key=$(date -r "$now_epoch" -v-"${offset}"d +%Y-%m-%d)
    daily_windows="${daily_windows}${key}|"
  done
  for ((offset = 1; offset <= 4; offset++)); do
    key=$(date -r "$now_epoch" -v-"${offset}"w +%G-W%V)
    weekly_windows="${weekly_windows}${key}|"
  done
  for ((offset = 1; offset <= 12; offset++)); do
    key=$(date -r "$now_epoch" -v-"${offset}"m +%Y-%m)
    monthly_windows="${monthly_windows}${key}|"
  done

  files=( "${snapshot_dir}"/org-*.zip )
  if [[ ! -e "${files[0]}" ]]; then
    return
  fi

  # Globbing orders epoch-based names oldest to newest. Walking backward makes
  # the first match the last available snapshot in each calendar period.
  for ((index = ${#files[@]} - 1; index >= 0; index--)); do
    file="${files[$index]}"
    base="${file##*/}"
    if [[ ! "$base" =~ ^org-([0-9]+)(-[[:xdigit:]]+)?\.zip$ ]]; then
      echo "Preserving unrecognized snapshot name: $file" >&2
      continue
    fi

    epoch="${BASH_REMATCH[1]}"
    keep=false

    # Preserve future-dated files rather than deleting data after clock skew.
    if ((epoch > now_epoch)); then
      keep=true
    fi

    if ((epoch >= recent_cutoff && epoch <= now_epoch)); then
      key=$(recent_key "$epoch")
      if ! key_is_in "$key" "$seen_recent"; then
        seen_recent="${seen_recent}${key}|"
        keep=true
      fi
    fi

    key=$(daily_key "$epoch")
    if key_is_in "$key" "$daily_windows" && ! key_is_in "$key" "$seen_daily"; then
      seen_daily="${seen_daily}${key}|"
      keep=true
    fi

    key=$(weekly_key "$epoch")
    if key_is_in "$key" "$weekly_windows" && ! key_is_in "$key" "$seen_weekly"; then
      seen_weekly="${seen_weekly}${key}|"
      keep=true
    fi

    key=$(monthly_key "$epoch")
    if key_is_in "$key" "$monthly_windows" && ! key_is_in "$key" "$seen_monthly"; then
      seen_monthly="${seen_monthly}${key}|"
      keep=true
    fi

    if [[ "$keep" == false ]]; then
      rm -f "$file"
    fi
  done
}

archive_previous_snapshot() {
  local epoch="$1"
  local target

  target=$(snapshot_target "$epoch" "$latest_archive")
  if [[ -e "$target" ]]; then
    return
  fi

  # latest_archive and target share a filesystem, so rename the old archive
  # instead of copying it.  Besides being atomic, this also works when a cloud
  # provider has evicted the archive's contents and left a dataless placeholder.
  mv "$latest_archive" "$target"
}

sync_current_files() {
  local status

  # A disposable, one-way copy for browsing in OneDrive. Keep deletion scoped
  # to this directory; never follow a replacement destination symlink.
  if [[ -L "$mirror_dir" ]]; then
    echo "Org mirror destination must not be a symlink: $mirror_dir" >&2
    return 1
  fi
  if ! mkdir -p "$mirror_dir"; then
    echo "Could not create Org mirror: $mirror_dir" >&2
    return 1
  fi

  # Preserve directory structure, symlinks, and timestamps without copying
  # Unix ownership or permissions into the cloud folder.
  if /usr/bin/rsync -rlt --delete \
    --exclude='.git' \
    --exclude='.DS_Store' \
    --exclude='.#*' \
    --exclude='[#]*[#]' \
    --exclude='*~' \
    "$source_dir/" "$mirror_dir/"; then
    echo "Synced Org files to $mirror_dir"
  else
    status=$?
    echo "Org mirror failed (rsync exit $status): $mirror_dir" >&2
    return "$status"
  fi
}

publish_readable_files() {
  local status=0
  local exporter="${ORG_READABLE_SCRIPT:-${HOME}/.emacs.d/bin/export-org-readable.sh}"

  # Both viewing copies are attempted after the archive work. Their failures
  # remain visible to launchd, and neither can block the other's next retry.
  sync_current_files || status=$?
  if ! /bin/bash "$exporter"; then
    echo "Org readable PDF export failed: $exporter" >&2
    status=1
  fi
  return "$status"
}

migrate_legacy_snapshots

# File names, sizes, modes, and modification times provide a quick change check.
source_fingerprint=$(
  LC_ALL=C /usr/bin/find -s "$source_dir" \
    -exec /usr/bin/stat -f '%N|%z|%m|%p' {} \; \
    | /usr/bin/shasum -a 256 \
    | /usr/bin/awk '{print $1}'
)

stored_fingerprint=""
previous_epoch=""
if [[ -f "$fingerprint_file" ]]; then
  stored_fingerprint=$(sed -n '1p' "$fingerprint_file")
  previous_epoch=$(sed -n '2p' "$fingerprint_file")
fi

if [[ -f "$latest_archive" && "$source_fingerprint" == "$stored_fingerprint" ]]; then
  prune_snapshots
  publish_readable_files
  exit 0
fi

source_parent=$(dirname "$source_dir")
source_name=$(basename "$source_dir")
(
  cd "$source_parent"
  /usr/bin/zip -qry "$temp_archive" "$source_name"
)

# Publish only a complete archive that passes zip's integrity check.
/usr/bin/zip -T "$temp_archive" >/dev/null

# Keep the prior latest only after its four-hour period has ended. The same
# historical file can later satisfy daily, weekly, and monthly retention.
if [[ -f "$latest_archive" && "$previous_epoch" =~ ^[0-9]+$ ]] \
  && [[ "$(recent_key "$previous_epoch")" != "$(recent_key "$now_epoch")" ]]; then
  archive_previous_snapshot "$previous_epoch"
fi

mv -f "$temp_archive" "$latest_archive"

state_temp="${state_dir}/.latest.$$.tmp"
{
  printf '%s\n' "$source_fingerprint"
  printf '%s\n' "$now_epoch"
} > "$state_temp"
mv -f "$state_temp" "$fingerprint_file"

prune_snapshots

echo "Updated $latest_archive"

# Archive creation and retention finish before mirroring, so a failed sync
# cannot prevent the backup. Also sync above when the archive is unchanged.
publish_readable_files
