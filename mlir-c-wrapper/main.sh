#!/bin/sh

set -ex

# TODO Install LLVM automatically when it's available.
# brew install llvm@15

mkdir -p tmp

mlir-reduce --convert-func-to-llvm foo.mlir >tmp/foo.mlir
mlir-translate --mlir-to-llvmir -o tmp/foo.ll tmp/foo.mlir
