#!/bin/bash

echo "Testing InternalEmoji.roc"
roc test package/InternalEmoji.roc

echo "Testing InternalGBP.roc"
roc test package/InternalGBP.roc

echo "Testing GraphemeTest.roc"
roc test package/GraphemeTest.roc

# for roc_file in **/*.roc; do
#     roc test $roc_file
# done
