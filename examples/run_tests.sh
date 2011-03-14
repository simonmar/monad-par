#!/bin/bash

# set -e 
set -o errexit

for file in "$@"; do
  echo Running test $file
  $file &> $file".out"
done
