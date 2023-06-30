(display (call/cc (lambda (k) 42)))

(display (call/cc (lambda (k) (k 42))))
