(require racket/control)

(define f
  (reset (+
    (begin (display 'a) 1)
    (begin (display 'b) (shift k k)))))

(display 'c)

(display (f 42))
