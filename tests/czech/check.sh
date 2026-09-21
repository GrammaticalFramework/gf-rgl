#!/bin/sh
set -eu
: "${GF:=gf}"
command -v "$GF" >/dev/null 2>&1 || {
  printf 'GF executable not found: %s\n' "$GF" >&2
  exit 1
}
cd "$(dirname "$0")/../.."
work=$(mktemp -d "${TMPDIR:-/tmp}/czech-rgl-tests.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
src=src/api:src/czech:src/common:src/abstract:src/prelude
mkdir -p "$work/source" "$work/api" "$work/consumer" "$work/pgf"
for suite in regressions; do
  "$GF" -run -path="$src" -gfo-dir="$work/source" \
    < "tests/czech/$suite.gfs" > "$work/$suite.out"
  diff -u "tests/czech/$suite.out" "$work/$suite.out"
done
# These are the normal Setup.hs language and API roots. AllCze includes ExtendCze.
"$GF" -c -path="$src" -gfo-dir="$work/api" \
  src/czech/AllCze.gf src/api/TryCze.gf src/api/SymbolicCze.gf \
  </dev/null > "$work/api.log" 2>&1 || { cat "$work/api.log"; exit 1; }
test -f "$work/api/ExtendCze.gfo"
# Compile a consumer with only the resulting distribution on its search path.
GF_LIB_PATH="$work/api" "$GF" -c -path="$work/api" -gfo-dir="$work/consumer" \
  tests/czech/CzeTests.gf \
  </dev/null > "$work/consumer.log" 2>&1 || { cat "$work/consumer.log"; exit 1; }
test -s "$work/consumer/CzeTests.gfo" || { cat "$work/consumer.log"; exit 1; }
 printf 'Czech source regressions, AllCze, installed API imports passed.\n'
