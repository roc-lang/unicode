#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output_dir="$root_dir/dist"
args=()

while (($# > 0)); do
    case "$1" in
        --output-dir)
            output_dir="$2"
            shift 2
            ;;
        --output-dir=*)
            output_dir="${1#--output-dir=}"
            shift
            ;;
        *)
            args+=("$1")
            shift
            ;;
    esac
done

mkdir -p "$output_dir"
output_dir="$(cd "$output_dir" && pwd)"

cd "$root_dir/package"

roc_files=(
    main.roc
    CodePoint.roc
    Grapheme.roc
    InternalEAW.roc
    InternalEmoji.roc
    InternalGBP.roc
)

if ((${#args[@]} > 0)); then
    roc bundle "${roc_files[@]}" --output-dir "$output_dir" "${args[@]}"
else
    roc bundle "${roc_files[@]}" --output-dir "$output_dir"
fi
