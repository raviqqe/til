#!/bin/sh

set -ex

brew install llvm@15

export PATH=$(brew --prefix llvm@15)/bin:$PATH

mkdir -p tmp

mlir-reduce --convert-func-to-llvm foo.mlir >tmp/foo.mlir
mlir-translate --mlir-to-llvmir -o tmp/foo.ll tmp/foo.mlir
