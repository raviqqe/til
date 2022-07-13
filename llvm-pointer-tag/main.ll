@a = global {i64} {i64 42}, align 8

define i64 @foo() {
  %x = inttoptr i64 and (i64 or (i64 ptrtoint ({i64}* @a to i64), i64 1), i64 -2) to {i64}*
  %y = load {i64}, {i64}* %x
  %z = extractvalue {i64} %y, 0
  ret i64 %z
}
