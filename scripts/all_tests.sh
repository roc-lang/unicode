#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

if [ -z "${ROC:-}" ]; then
  echo "ERROR: The ROC environment variable is not set.
    Set it to something like:
        /path/to/the/compiler-named-in-.roc-version/roc
        or
        /home/username/gitrepos/roc/zig-out/bin/roc" >&2

  exit 1
fi

python3 scripts/test.py all --roc "$ROC"

for roc_file in package/*.roc; do
    "$ROC" check "$roc_file" --no-cache
done

for roc_file in package/*.roc; do
    if grep -q '^expect' "$roc_file"; then
        "$ROC" test "$roc_file" --no-cache
    fi
done

"$ROC" docs package/main.roc
