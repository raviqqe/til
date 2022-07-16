#!/bin/sh

set -ex

. $(dirname $0)/../tools/utilities.sh

test_rust

cargo build --release
objdump -d target/release/sum >sum.s
