#!/bin/sh

set -ex

brew install llvm@15

export PATH=$(brew --prefix llvm@15)/bin:$PATH

mlir-translate --mlir-to-llvmir -o foo.ll foo.mlir
