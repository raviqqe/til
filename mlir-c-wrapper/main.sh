#!/bin/sh

set -ex

brew install llvm@14

mlir-translate --mlir-to-llvmir -o foo.ll foo.mlir
