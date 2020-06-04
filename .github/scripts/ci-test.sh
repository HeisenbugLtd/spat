#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

echo "Running tests:"
./obj/run_spat -P spat.gpr
# Not a real test.
# Run on itself. There's no .spark files, so no output.
./obj/run_spat -h
# Show help.
