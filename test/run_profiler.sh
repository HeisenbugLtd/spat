#!/usr/bin/env bash

#
# Build and run a profiled executable.
#

gprbuild -f -P ../spat.gpr -cargs -pg -largs -pg || exit 1
./run_test.sh
gprof ../obj/run_spat gmon.out > profile-run.txt
gprbuild -f -P ../spat.gpr
