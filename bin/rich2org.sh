#!/usr/bin/env bash
# rich2org.sh — Convert clipboard rich text (HTML) to Org mode
# Requires: pandoc
#
# Usage:
#   rich2org.sh           → prints Org to stdout
#   rich2org.sh -i        → inserts into current tmux pane (for terminal use)
#   rich2org.sh -o FILE   → writes to file

set -euo pipefail

TMPFILE=$(mktemp /tmp/rich2org_XXXXXX.html)
trap 'rm -f "$TMPFILE"' EXIT

# Extract HTML from macOS clipboard
osascript -e 'use framework "AppKit"' \
  -e 'set pb to current application'\''s NSPasteboard'\''s generalPasteboard()' \
  -e 'set htmlData to pb'\''s dataForType:"public.html"' \
  -e 'if htmlData is missing value then return ""' \
  -e 'set nsStr to current application'\''s NSString'\''s alloc()'\''s initWithData:htmlData encoding:(current application'\''s NSUTF8StringEncoding)' \
  -e 'return nsStr as text' > "$TMPFILE"

if [[ ! -s "$TMPFILE" ]]; then
    echo "Error: No rich text found on clipboard. Copy something from Outlook or OneNote first." >&2
    exit 1
fi

ORG=$(pandoc -f html -t org --wrap=none "$TMPFILE" \
  | grep -v 'data:image' \
  | sed '/:PROPERTIES:/,/:END:/d' \
  | sed 's/^\(#+begin_src\) .*/\1/' \
  | sed $'s/\xC2\xA0/ /g' \
  | sed 's/  */ /g' \
  | awk 'NF{blank=0} !NF{blank++} blank<=1')

case "${1:-}" in
    -o)
        echo "$ORG" > "${2:?Usage: rich2org.sh -o OUTPUT_FILE}"
        echo "Written to $2"
        ;;
    *)
        echo "$ORG"
        ;;
esac
