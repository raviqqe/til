#!/bin/sh

set -ex

brew install llvm

opt -O3 main.ll | llvm-dis
