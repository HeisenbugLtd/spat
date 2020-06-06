#!/usr/bin/env bash

# Regression tests

# Check out a known project, run gnatprove on it, and compare the results of
# a spat run with stored template.

TEST_PROJECT="https://github.com/HeisenbugLtd/Saatana"
TEST_VERSION="v2.0.2"
REPO_DIR="test-saatana"

# Remove possibly leftover temp files and checked out test repository
rm -f test.diff *.out
rm -rf $REPO_DIR

# Prepare test repository
git clone $TEST_PROJECT $REPO_DIR
cd $REPO_DIR
git checkout -d $TEST_VERSION
cd ..
gnatprove --steps=1 -P $REPO_DIR/saatana.gpr

# run the freshly built executable on the files and store the result in spat.out
# -s  for summary
# -l  for list
# -d  for details
# -ct sort by time

for SPAT_OPTIONS in "-s -l -d -ct" "-s -l -d -ca"; do
  ../obj/run_spat -s -l -d -ct -P $REPO_DIR/saatana.gpr | sed -f filter.sed > "spat.$SPAT_OPTIONS.out"
  # Show template differences
  diff -u "spat.$SPAT_OPTIONS.template" "spat.$SPAT_OPTIONS.out" | tee -a test.diff
done

if [ -s test.diff ]; then
  echo "Test failed, there are differences."
else
  echo "Test succeeded."
fi

# Remove temp files and checked out test repository
rm -f test.diff *.out
rm -rf $REPO_DIR
