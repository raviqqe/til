#!/bin/sh

set -ex

for basename in heterogeneous_struct large_struct spill spill_large_struct; do
  compile="clang -S -O3 $basename.c"

  $compile -emit-llvm
  $compile
done
