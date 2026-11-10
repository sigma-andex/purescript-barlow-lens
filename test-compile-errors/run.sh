#!/usr/bin/env bash
# Checks that barlow's custom compile errors are reported.
#
# Every *.purs file in this directory must FAIL to compile, and the compiler output
# must contain the text from its first line (`-- expect: <text>`).
#
# Run from the repository root after `spago build` (which installs the packages into .spago):
#   bash test-compile-errors/run.sh
# Set OUTPUT to change the output directory (default: output, reusing spago's build cache).

set -uo pipefail

cd "$(dirname "$0")/.."
output="${OUTPUT:-output}"
failures=0
count=0

for file in test-compile-errors/*.purs; do
  count=$((count + 1))
  name=$(basename "$file" .purs)
  expected=$(head -n 1 "$file" | sed -n 's/^-- expect: //p')
  if [ -z "$expected" ]; then
    echo "✗ $name: missing '-- expect: ...' on the first line"
    failures=$((failures + 1))
    continue
  fi

  if result=$(purs compile -o "$output" '.spago/p/*/src/**/*.purs' 'src/**/*.purs' "$file" 2>&1); then
    echo "✗ $name: compiled, but should have failed with: $expected"
    failures=$((failures + 1))
  elif grep -qF -- "$expected" <<< "$result"; then
    echo "✓ $name"
  else
    echo "✗ $name: expected an error containing: $expected"
    echo "$result" | awk '/^Error/{p=1} p' | head -20 | sed 's/^/    /'
    failures=$((failures + 1))
  fi
done

echo
echo "$((count - failures))/$count compile error checks passed"
[ "$failures" -eq 0 ]
