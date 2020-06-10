#!/usr/bin/env bash

# Change the version information in src/spat-version.ads

# TODO: Sanity check of version.

VFILE="src/spat-version.ads"

sed 's/^\(\ \+Number\ \+:\ \+constant\ \+String\ \+:=\ \+\"\)\([0-9a-z.-]*\)\(\";\)/\1'$1'\3/' $VFILE > $VFILE.new || exit 1
cat $VFILE.new || exit 2

read -p "Does the file look ok?\nPress Ctrl-C to abort operation, ENTER to continue..." || exit 255

# Overwrite target file.
mv $VFILE.new $VFILE

# Recompile
gprbuild -f -P spat.gpr || exit 3

# TODO: Run executable and check that it reports the expected version.

read -p "Last chance. If you really want me to commit and tag \"v$1\", press ENTER, Ctrl-C to abort" || exit 255

# Compilation successful, add the file and commit, and then tag the version
git commit --dry-run $VFILE -m "* Automated tagging of $1 version."
read -p "Does the dry run look ok to you? Press ENTER to continue, Ctrl-C to abort" || exit 255

git commit $VFILE -m "* Automated tagging of $1 version." || exit 4
git tag v$1 || exit 5

echo "Version \"v$1\" tagged. You may want to do 'git push --tags', though."
