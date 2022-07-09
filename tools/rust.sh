#!/bin/sh

set -ex

cargo build
cargo test
cargo clippy -- -D warnings
cargo fmt --check
