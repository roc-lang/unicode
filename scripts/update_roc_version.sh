#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

if [ "$#" -ne 1 ]; then
    echo "Usage: ./scripts/update_roc_version.sh NEW_ROC_VERSION" >&2
    echo "Example: ./scripts/update_roc_version.sh nightly-2026-08-11-56acb9b" >&2
    exit 1
fi

readonly new_version="$1"

if [[ ! "$new_version" =~ ^nightly-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9a-f]+$ ]]; then
  echo "expected a nightly release tag like nightly-2026-08-10-7df8509, got: $new_version" >&2
  exit 1
fi

echo "$new_version" > .roc-version

echo "Pinned Roc version updated to $new_version."
