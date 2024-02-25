#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

if [ -z "${ROC}" ]; then
  echo "ERROR: The ROC environment variable is not set.
    Set it to something like:
        /home/username/Downloads/roc_nightly-linux_x86_64-2023-10-30-cb00cfb/roc
        or
        /home/username/gitrepos/roc/target/build/release/roc" >&2

  exit 1
fi

examples_dir='./examples/'
package_dir='./package/'

# roc check
echo "Checking examples files..."
for roc_file in $examples_dir*.roc; do
    $ROC check $roc_file
done

# roc build
echo "Checking examples builds..."
for roc_file in $examples_dir*.roc; do
    $ROC build $roc_file --linker=legacy
done

# roc check
echo "Checking package files..."
for roc_file in $package_dir*.roc; do
    $ROC check $roc_file
done

# roc test
echo "Running test for package... (only files containing expect)"
for roc_file in $package_dir*.roc; do
    if grep -q '^expect' "$roc_file" >/dev/null; then
        echo "Running tests for $roc_file"
        $ROC test $roc_file
    fi
done

# test building docs website
$ROC docs package/main.roc