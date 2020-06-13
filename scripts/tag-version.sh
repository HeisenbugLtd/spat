#!/usr/bin/env bash

# Change the version information in src/spat-version.ads

# TODO: Sanity check of version.

VERSION_NUMBER=$1

if [ "$#" -ne 1 ]; then
    echo "No or more than one version number given. Please provide exactly one."
    exit 1
fi

# Validate input version number.

if [ -z `echo $VERSION_NUMBER | grep -P '^(\d+\.)+(\d+\.)+(\d+)+([-][a-z]*)?$'` ]; then
    echo "Version number \"${VERSION_NUMBER}\" is not in format d.d.d[-suffix]"
    exit 1
fi

TAGS=`git tag | grep -P "^v${VERSION_NUMBER}$"`

if [ -n "${TAGS}" ]; then
    echo "Version number \"${VERSION_NUMBER}\" has already been used."
    exit 1
fi

VFILE="src/spat-version.ads"

sed 's/^\(\ \+Number\ \+:\ \+constant\ \+String\ \+:=\ \+\"\)\([0-9a-z.-]*\)\(\";\)/\1'${VERSION_NUMBER}'\3/' $VFILE > $VFILE.new || exit 1
cat $VFILE.new || exit 2

read -p "Does the file look ok? Press Ctrl-C to abort operation, ENTER to continue..." || exit 255

# Overwrite target file.
mv $VFILE.new $VFILE

# Recompile
gprbuild -f -P spat.gpr || exit 3

# TODO: Run executable and check that it reports the expected version.

echo "Latest version number used was \"`git tag --sort=\"creatordate\" | tail -1`\", new one is \"v${VERSION_NUMBER}\"."
read -p "Last chance. If you really want me to commit and tag \"v${VERSION_NUMBER}\", press ENTER, Ctrl-C to abort" || exit 255

# Compilation successful, add the file and commit, and then tag the version
git commit --dry-run $VFILE -m "* Automated tagging of ${VERSION_NUMBER} version."
read -p "Does the dry run look ok to you? Press ENTER to continue, Ctrl-C to abort" || exit 255

git commit $VFILE -m "* Automated tagging of ${VERSION_NUMBER} version." || exit 4
git tag v${VERSION_NUMBER} || exit 5

echo "Version \"v${VERSION_NUMBER}\" tagged. You may want to do 'git push --tags', though."
