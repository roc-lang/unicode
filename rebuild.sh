#!/bin/bash

# Generate the GBP internal module
echo "Generating InternalGBP.roc"
roc run package/InternalGBPGen.roc -- package/

# Test the GBP internal module
echo "Testing InternalGBP.roc"
roc test package/InternalGBP.roc

# Generate the GraphemeTest module
echo "Generating GraphemeTest.roc"
roc run package/GraphemeTestGen.roc -- package/

# Test the GraphemeTest module
echo "Testing GraphemeTest.roc"
roc test package/GraphemeTest.roc
