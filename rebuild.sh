#!/bin/bash

echo "Generating InternalEmoji.roc"
roc package/InternalEmojiGen.roc > package/InternalEmoji.roc

echo "Generating InternalGBP.roc"
roc package/InternalGBPGen.roc > package/InternalGBP.roc

echo "Generating GraphemeTest.roc"
roc package/GraphemeTestGen.roc > package/GraphemeTest.roc

echo "Generating InternalEAW.roc"
roc package/InternalEAWGen.roc > package/InternalEAW.roc
