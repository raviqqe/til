module {
	llvm.func @foo(
	  %arg0: !llvm.ptr<struct<(ptr<f32>, ptr<f32>, i64, array<2xi64>, array<2xi64>)>>
	) attributes { llvm.emit_c_interface } {
		llvm.return
	}
}
