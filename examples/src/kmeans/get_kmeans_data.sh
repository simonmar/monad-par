#!/bin/bash

DAT=kmeans_data.tbz

if ! [ -f $DAT ]; then
  wget http://cs.indiana.edu/~rrnewton/datasets/$DAT
  tar xjvf $DAT
else
  echo "  $DAT already exists!"
fi
