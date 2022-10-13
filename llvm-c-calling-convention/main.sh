#!/bin/sh

set -ex

for basename in heterogeneous_struct large_struct spill spill_large_struct; do
  compile="clang -S -O3 $basename.c"

  $compile -emit-llvm

  for target in i386 x86_64 arm aarch64; do
    $compile -target $target -o $basename.$target.s
  done
done
