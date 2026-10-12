#!/bin/sh

set -e

cargo build --release
ls -l target/release/rust-no-libc
target/release/rust-no-libc
