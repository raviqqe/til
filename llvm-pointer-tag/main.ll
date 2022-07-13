@a = global {i64 ()*} {i64 ()* @bar}

define void @foo() {
  ret void
}

define i64 @bar() {
  ret i64 42
}
