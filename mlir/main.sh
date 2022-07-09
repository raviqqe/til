#!/bin/sh

set -ex

mlir-translate --mlir-to-llvmir -o foo.ll foo.mlir
