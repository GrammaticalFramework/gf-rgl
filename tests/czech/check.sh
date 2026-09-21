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
for suite in regressions markup; do
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
# Acyclic abstract grammars make the complete parse sets finite. Compare
# whole trees without relying on parser enumeration order; repeated strings in
# the treebank record genuine ambiguity, including polite/plural address.
for grammar in CzeRoundTrip; do
case "$grammar" in
  CzeRoundTrip) bank=tests/czech/roundtrip.tsv ;;
  CzeExtensionRoundTrip) bank=tests/czech/extension-roundtrip.tsv ;;
esac
"$GF" -make -path="$src":tests/czech -gfo-dir="$work/api" -output-dir="$work/pgf" \
  "tests/czech/${grammar}Cze.gf" \
  </dev/null > "$work/pgf.log" 2>&1 || { cat "$work/pgf.log"; exit 1; }
while IFS="$(printf '\t')" read -r tree surface; do
  printf 'i %s\nl -lang=%sCze %s\np -lang=%sCze -cat=Utt "%s"\nq\n' \
    "$work/pgf/$grammar.pgf" "$grammar" "$tree" "$grammar" "$surface" \
    | "$GF" -run > "$work/roundtrip.out"
  sed '/^$/d' "$work/roundtrip.out" > "$work/roundtrip.lines"
  sed -n '1p' "$work/roundtrip.lines" > "$work/linearization.out"
  printf '%s\n' "$surface" > "$work/linearization.expected"
  diff -u "$work/linearization.expected" "$work/linearization.out"
  sed '1d' "$work/roundtrip.lines" | LC_ALL=C sort > "$work/parses.out"
  awk -F '\t' -v surface="$surface" '$2 == surface {print $1}' "$bank" \
    | LC_ALL=C sort > "$work/parses.expected"
  diff -u "$work/parses.expected" "$work/parses.out"
done < "$bank"
done
printf 'Czech source regressions, AllCze, installed API imports and PGF round trips passed.\n'
