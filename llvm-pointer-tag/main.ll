declare i8* @llvm.ptrmask.p0i8.i64(i8* , i64)

@a = unnamed_addr constant {i64} {i64 42}, align 8

define i64 @foo() {
  %p = ptrtoint {i64}* @a to i64
  %s = or i64 %p, 1
  %t = and i64 %s, -2
  %x = inttoptr i64 %t to {i64}*
  %y = load {i64}, {i64}* %x
  %z = extractvalue {i64} %y, 0
  ret i64 %z
}

define i64 @bar() {
  %p = bitcast {i64}* @a to i8*
  %s = getelementptr i8, i8* %p, i64 1
  %t = call i8* @llvm.ptrmask.p0i8.i64(i8* %s, i64 18446744073709551608)
  %x = bitcast i8* %t to {i64}*
  %y = load {i64}, {i64}* %x
  %z = extractvalue {i64} %y, 0
  ret i64 %z
}
