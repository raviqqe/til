@foo = unnamed_addr constant i32 0
@bar = internal unnamed_addr constant i32 0

define i32* @baz() {
  ret i32* @bar 
}
