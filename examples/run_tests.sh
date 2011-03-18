#!/bin/bash

# set -e 
# set -o errexit

for file in "$@"; do
  echo Running test $file
  ./$file &> $file".out"
#  echo "Return code: $?"
  code=$?
  if [ "$code" != 0 ]; then 
    echo
    echo "  ERROR, output was:"
    cat $file".out"
    echo
    echo "  ERROR: exit code $code"
    exit $code
  fi
done
