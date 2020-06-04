#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

echo "Running tests:"

# Download and install dependencies
#git clone https://github.com/HeisenbugLtd/si_units
#cd si_units
#git checkout v0.1.0
#cd ..
gprbuild -p -P si_units/si_units_lib.gpr
gprinstall -v -p -P si_units/si_units_lib.gpr

# debug, list what's available
gprinstall --list --stat

# Not a real test.
# Run on itself. There's no .spark files, so no output.
./obj/run_spat -P spat.gpr

# Show help output.
./obj/run_spat -h
