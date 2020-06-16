#!/usr/bin/env bash

# Change the version information in src/spat-version.ads

if [ "$#" -ne 1 ]; then
    echo "No or more than one version number given. Please provide exactly one."
    exit 1
fi

VERSION="$1"
TAG="v${VERSION}"

# 
confirm()
{
    echo "$1"
    read -p "Ctrl-C to abort, ENTER to continue..." || exit 255
}

# Validate input version number.

# Check format
if [ -z `echo "${VERSION}" | grep -P '^(\d+\.)+(\d+\.)+(\d+)+([-][a-z0-9]+)?$'` ]; then
    echo "Version number \"${VERSION}\" is not in format d.d.d[-suffix]"
    exit 2
fi

# Check if it's unique
TAGS=`git tag | grep -P "^${TAG}$"`

if [ -n "${TAGS}" ]; then
    echo "Version number \"${VERSION}\" has already been used."
    exit 3
fi

echo "Latest version tag used was \"`git tag --sort=\"creatordate\" | tail -1`\", new one is \"${TAG}\"."
confirm "Does that look fine to you?"

# Patch file with new version number
VFILE="src/spat-version.ads"

sed 's/^\(\ \+Number\ \+:\ \+constant\ \+String\ \+:=\ \+\"\)\([0-9a-z.-]*\)\(\";\)/\1'${VERSION}'\3/' "${VFILE}" > "${VFILE}.new" || (echo "Could not patch file. Are you in the root directory?"; exit 4)

cat "${VFILE}.new" || (echo "Could not display file. That's odd."; exit 5)
confirm "This would be the new version file."

# Overwrite target file.
mv "${VFILE}.new" "${VFILE}" || (echo "Failure to move file to final location!"; exit 6)

# Recompile
gprbuild -f -P spat.gpr || (echo "Compilation failed!"; exit 7)

# TODO: Run executable and check that it reports the expected version.

confirm "Last chance. Do you really want me to commit and tag \"${TAG}\"?"

# Compilation successful, add the file and commit, and then tag the version
git commit --dry-run "${VFILE}" -m "* Automated tagging of ${VERSION} version."
confirm "Does the dry run look ok to you?"

git commit "${VFILE}" -m "* Automated tagging of ${VERSION} version." || exit 4
git tag "${TAG}" || exit 5

echo "Version \"${VERSION}\" tagged as \"${TAG}\". You may want to do 'git push --tags', though."

# Temporarily create the tarball and sign it.
echo -n "Signing tar ball..."
TMPDIR=`mktemp -d`
TARFILE="${TMPDIR}/${TAG}.tar.gz"
SIGFILE=`basename "${TARFILE}.asc"`

git archive --prefix="${TAG}/" -o "${TARFILE}" "${TAG}"
gpg --armor --detach-sign "${TARFILE}"
mv "${TMPDIR}/${SIGFILE}" "./${SIGFILE}"
rm -rf "${TMPDIR}"
echo "signature file \"${SIGFILE}\" (hopefully) created."
