#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

roc='./roc_nightly/roc'

examples_dir='./examples/'
package_dir='./package/'

# roc check
echo "Checking examples files..."
for roc_file in $examples_dir*.roc; do
    $roc check $roc_file
done

# roc build
echo "Checking examples builds..."
for roc_file in $examples_dir*.roc; do
    $roc build $roc_file --linker=legacy
done

# roc check
echo "Checking package files..."
for roc_file in $package_dir*.roc; do
    $roc check $roc_file
done

# roc test
echo "Running test for package... (only files containing expect)"
for roc_file in $package_dir*.roc; do
    if grep -q '^expect' "$roc_file" >/dev/null; then
        echo "Running tests for $roc_file"
        $roc test $roc_file
    fi
done

# test building docs website
$roc docs package/main.roc