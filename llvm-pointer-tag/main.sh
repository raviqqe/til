#!/bin/sh

set -ex

brew install llvm

llc -O3 main.ll

for options in '-o main_c.s' '-emit-llvm -o main_c.ll'; do
  clang -c -S -O3 $options main.c
done
