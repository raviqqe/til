(define (f x) (+ x x))

(define (i x) x)

(define-syntax call-me-maybe
  (syntax-rules ()
    ((call-me-maybe f x) (f x))))

(display (call-me-maybe f 42))
