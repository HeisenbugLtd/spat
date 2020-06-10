#!/usr/bin/env bash

#
# Build and run a profiled executable.
#

SPAT_OPTIONS="-s -l -d -ct" # Summary, list, details, sort by time

echo "Building instrumented executable..."
gprbuild -f -P ../spat.gpr -cargs -pg -largs -pg || exit 1

echo "Running..."

# Run on test data 1
../obj/run_spat $SPAT_OPTIONS -P test-saatana/saatana.gpr > /dev/null
gprof ../obj/run_spat gmon.out > profile-run-1.txt

# Run on test data 2
../obj/run_spat $SPAT_OPTIONS -P test-sparknacl/src/sparknacl.gpr > /dev/null
gprof ../obj/run_spat gmon.out > profile-run-2.txt

echo "Rebuilding standard executable."
gprbuild -f -P ../spat.gpr
