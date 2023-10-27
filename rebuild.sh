#!/bin/bash

echo "Generating InternalEmoji.roc"
roc run package/InternalEmojiGen.roc -- package/

echo "Testing InternalEmoji.roc"
roc test package/InternalEmoji.roc

echo "Generating InternalGBP.roc"
roc run package/InternalGBPGen.roc -- package/

echo "Testing InternalGBP.roc"
roc test package/InternalGBP.roc

echo "Generating GraphemeTest.roc"
roc run package/GraphemeTestGen.roc -- package/

echo "Testing GraphemeTest.roc"
roc test package/GraphemeTest.roc
