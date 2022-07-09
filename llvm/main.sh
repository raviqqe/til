#!/bin/sh

set -ex

brew install llvm@14

llc -O3 large_struct.ll

for basename in large_struct spill spill_large_struct; do
  for options in "-o ${basename}_c.s" "-emit-llvm -o ${basename}_c.ll"; do
    clang $options -S -O3 $basename.c
  done
done
