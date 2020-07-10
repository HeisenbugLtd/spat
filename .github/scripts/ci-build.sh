#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

# For the record
echo ENVIRONMENT:
env | sort
echo ............................

echo GNAT VERSION:
gnatls -v
echo ............................

# Download and install dependencies
git clone https://github.com/HeisenbugLtd/si_units
cd si_units
git checkout v0.1.1
cd ..
gprbuild -p -P si_units/si_units_lib.gpr
gprinstall -v -p -P si_units/si_units_lib.gpr

# debug, list what's available
gprinstall --list --stat

# Build library project and then test programs
gprbuild -p -P spat.gpr
