#!/bin/bash

# Search for the .git folder in a haskell subtree on the users home dir
GIT_HASKELL_FOLDER="$(find ~ -name '.git' | grep -i 'haskell')"
if [ -z "${GIT_HASKELL_FOLDER}" ]; then
    echo "No Haskell Git folder found in user $(whoami) homedir!" >2
    exit 1
fi

HASKELL_FOLDER="$(dirname "${GIT_HASKELL_FOLDER}")"
# get the week directories
WORKING_DIRS="$(mktemp)" 
find -P "${HASKELL_FOLDER}" -mindepth 1 -maxdepth 1 -type d -name 'week*' > "${WORKING_DIRS}"

# for each week directory in the haskell project, remove the files which are not .hs (Haskell source) -> .obj and binaries
while read -r WORKING_DIR; do
    # from the weekN directory, find all normal files which don't have extension .hs in their names
    # and execute a single rm command on all of them. * You have to escape the !, name_pattern and {}
    # in order for BASH not to interpret them instead of find command
    find "${WORKING_DIR}" -type f \! -name '*\.hs' -exec rm '{}' +
done < "${WORKING_DIRS}"
rm "${WORKING_DIRS}"
exit 0