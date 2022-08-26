#!/bin/bash
set -e
if [[ $# -eq 0 ]] ; then
    echo 'a string must be supplied'
    exit 1
fi

# 12345678901234567890123456789012
# 3132333435363738393031323334353637383930313233343536373839303132
value=${1}
value=$(echo ${value:0:32})
echo $(echo -n ${value} | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')


