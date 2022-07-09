#!/bin/sh

set -ex

brew install llvm@14

llc -O3 foo.ll
clang -S -O3 bar.c
