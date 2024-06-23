#!/bin/bash

echo "Generating InternalEmoji.roc"
roc run package/InternalEmojiGen.roc -- package/

echo "Generating InternalGBP.roc"
roc run package/InternalGBPGen.roc -- package/

echo "Generating GraphemeTest.roc"
roc run package/GraphemeTestGen.roc -- package/

echo "Generating InternalEAW.roc"
roc run package/InternalEAWGen.roc -- package/
