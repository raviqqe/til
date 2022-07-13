@a = constant {i64} {i64 42}, align 8

define i64 @foo() {
  %p = and i64 ptrtoint ({i64}* @a to i64), -2
  %s = or i64 %p, 1
  %t = and i64 %s, -2
  %x = inttoptr i64 %t to {i64}*
  %y = load {i64}, {i64}* %x
  %z = extractvalue {i64} %y, 0
  ret i64 %z
}
