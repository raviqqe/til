test_rust() {
  cargo build
  cargo test
  cargo clippy -- -D warnings
  cargo fmt --check
}
