#!/bin/sh

set -ex

brew install llvm

export PATH=$(brew --prefix llvm)/bin:$PATH

llc -O3 main.ll
