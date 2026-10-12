#!/bin/sh

set -e

binary=target/release/rust-no-libc

cargo build --release
ls -l $binary
$binary
