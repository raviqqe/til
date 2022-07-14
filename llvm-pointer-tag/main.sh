#!/bin/sh

set -ex

brew install llvm

llc -O3 main.ll
clang -c -S -O3 -o main_c.s main.c
