#!/bin/sh

set -ex

for basename in large_struct spill spill_large_struct; do
  for options in '' -emit-llvm; do
    clang $options -S -O3 $basename.c
  done
done
