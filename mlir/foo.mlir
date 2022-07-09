module {
	// TODO Use func.func.
	llvm.func @foo(
	  %arg0: !llvm.struct<(ptr<f32>, ptr<f32>, i64, array<2xi64>, array<2xi64>)>
	) attributes { llvm.emit_c_interface } {
		// TODO Use func.return.
		llvm.return
	}
}
