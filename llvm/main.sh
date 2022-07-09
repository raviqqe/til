#!/bin/sh

set -ex

brew install llvm@14

llc -S -O3 foo.ll
