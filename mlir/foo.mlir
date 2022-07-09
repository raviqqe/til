module {
	llvm.func @foo(
	  %arg0: !llvm.ptr<struct<(ptr<f32>, ptr<f32>, i64, array<2xi64>, array<2xi64>)>>
	) {
		llvm.return
	}
}
