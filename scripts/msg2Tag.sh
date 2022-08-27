#!/bin/bash
set -e
if [[ $# -eq 0 ]] ; then
    echo 'a string must be supplied'
    exit 1
fi

# replace commas with proper commas
string=${1}
string_to_replace="'"
replacing_string="\'"
string=$(echo ${string/${string_to_replace}/${replacing_string}})

python3 -c "from py.textToTag import updateTagDatum;updateTagDatum('${string}')"