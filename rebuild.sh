#!/bin/bash

echo "Generating InternalEmoji.roc"
../roc/target/release/roc package/InternalEmojiGen.roc > package/InternalEmoji.roc

echo "Generating InternalGBP.roc"
../roc/target/release/roc package/InternalGBPGen.roc > package/InternalGBP.roc

echo "Generating GraphemeTest.roc"
../roc/target/release/roc package/GraphemeTestGen.roc > package/GraphemeTest.roc

echo "Generating InternalEAW.roc"
../roc/target/release/roc package/InternalEAWGen.roc > package/InternalEAW.roc
