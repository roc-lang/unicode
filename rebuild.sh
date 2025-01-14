#!/bin/bash

echo "Generating InternalEmoji.roc"
roc package/InternalEmojiGen.roc -- package/

echo "Generating InternalGBP.roc"
roc package/InternalGBPGen.roc -- package/

echo "Generating GraphemeTest.roc"
roc package/GraphemeTestGen.roc -- package/

echo "Generating InternalEAW.roc"
roc package/InternalEAWGen.roc -- package/
