#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output_dir="$root_dir/dist"
args=()
roc_bin="${ROC:-roc}"

pinned_roc="$(sed -n '1p' "$root_dir/.roc-version")"
actual_roc="$("$roc_bin" version)"
if [[ "$actual_roc" != *"$pinned_roc"* ]]; then
    echo "ERROR: repository requires $pinned_roc, got '$actual_roc'" >&2
    exit 1
fi

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
    ByteRange.roc
    CodePoint.roc
    Grapheme.roc
    UnicodeVersion.roc
    InternalGrapheme.roc
    InternalGraphemeData.roc
    InternalUtf8.roc
    InternalEAW.roc
    InternalEmoji.roc
    InternalGBP.roc
)

if ((${#args[@]} > 0)); then
    "$roc_bin" bundle "${roc_files[@]}" --output-dir "$output_dir" "${args[@]}"
else
    "$roc_bin" bundle "${roc_files[@]}" --output-dir "$output_dir"
fi
