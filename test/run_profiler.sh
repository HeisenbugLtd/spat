#!/usr/bin/env bash

#
# Build and run a profiled executable.
#
SPAT="../obj/run_spat"
SPAT_OPTIONS="-s -ra -d -ct" # Summary, report all, details, sort by time

echo "Building instrumented executable..."
gprbuild -f -P ../spat.gpr -cargs -pg -largs -pg || exit 1

rm -f gmon.*

echo "Running test run 1..."
$SPAT $SPAT_OPTIONS -P test-saatana/saatana.gpr > /dev/null

echo "Collecting data..."
gprof $SPAT gmon.out > profile-run-1.txt
rm -f gmon.*

echo "Running test run 2..."
$SPAT $SPAT_OPTIONS -P test-sparknacl/src/sparknacl.gpr > /dev/null

echo "Collecting data..."
gprof $SPAT gmon.out > profile-run-2.txt
rm -f gmon.*

echo "Rebuilding standard executable."
gprbuild -f -P ../spat.gpr
