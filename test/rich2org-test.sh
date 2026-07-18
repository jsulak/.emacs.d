#!/usr/bin/env bash

set -euo pipefail

ROOT_DIRECTORY=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TEST_DIRECTORY=$(mktemp -d "${TMPDIR:-/tmp}/rich2org-test.XXXXXX")
FAKE_BIN="$TEST_DIRECTORY/bin"
SCRIPT="$ROOT_DIRECTORY/bin/rich2org.sh"

cleanup() {
    rm -rf "$TEST_DIRECTORY"
}
trap cleanup EXIT

mkdir -p "$FAKE_BIN"

cat > "$FAKE_BIN/osascript" <<'EOF'
#!/usr/bin/env bash
printf '%s' "${FAKE_HTML:-}"
EOF

cat > "$FAKE_BIN/pandoc" <<'EOF'
#!/usr/bin/env bash
printf '%s' "${FAKE_PANDOC_OUTPUT:-}"
EOF

chmod +x "$FAKE_BIN/osascript" "$FAKE_BIN/pandoc"

assert_equal() {
    local expected=$1
    local actual=$2
    local description=$3
    if [[ $actual != "$expected" ]]; then
        printf 'FAIL: %s\nExpected:\n%s\nActual:\n%s\n' \
            "$description" "$expected" "$actual" >&2
        exit 1
    fi
}

converted=$(env \
    PATH="$FAKE_BIN:$PATH" \
    FAKE_HTML='<p>clipboard</p>' \
    FAKE_PANDOC_OUTPUT=$'Heading\n\n\n![image](data:image/png;base64,abc)\n:PROPERTIES:\n:ID: 1\n:END:\n#+begin_src python\nprint(1)\n' \
    "$SCRIPT")
assert_equal $'Heading\n\n#+begin_src\nprint(1)' "$converted" \
    'conversion filters metadata and normalizes whitespace'

output_file="$TEST_DIRECTORY/result.org"
message=$(env \
    PATH="$FAKE_BIN:$PATH" \
    FAKE_HTML='<p>clipboard</p>' \
    FAKE_PANDOC_OUTPUT='Converted' \
    "$SCRIPT" -o "$output_file")
assert_equal "Written to $output_file" "$message" 'output-file confirmation'
assert_equal 'Converted' "$(<"$output_file")" 'output-file contents'

if env PATH="$FAKE_BIN:$PATH" FAKE_HTML='' "$SCRIPT" \
    >"$TEST_DIRECTORY/no-html.out" 2>"$TEST_DIRECTORY/no-html.err"; then
    printf 'FAIL: empty clipboard should fail\n' >&2
    exit 1
fi
grep -q 'No rich text found' "$TEST_DIRECTORY/no-html.err"

if "$SCRIPT" --invalid >"$TEST_DIRECTORY/usage.out" 2>"$TEST_DIRECTORY/usage.err"; then
    printf 'FAIL: invalid arguments should fail\n' >&2
    exit 1
fi
grep -q '^Usage:' "$TEST_DIRECTORY/usage.err"

printf 'rich2org shell tests passed\n'
