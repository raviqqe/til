(display (call/cc (lambda (k) (begin (k 1) (k 2) k))))
