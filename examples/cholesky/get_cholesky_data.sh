#!/bin/bash

# wget http://people.csail.mit.edu/~newton/haskell-cnc/datasets/cholesky_matrix_data.tbz

DAT=cholesky_matrix_data.tbz

if ! [ -f $DAT ]; then
  wget http://cs.indiana.edu/~rrnewton/datasets/$DAT
  tar xjvf $DAT
else
  echo "  $DAT already exists!"
fi
