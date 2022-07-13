#!/bin/sh

set -ex

brew install llvm

llc -O3 main.ll
