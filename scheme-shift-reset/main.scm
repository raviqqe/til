(require racket/control)

(display (reset (+ 1 (shift k 41))))

(define f
  (reset (+
    (begin (display 'a) 1)
    (begin (display 'b) (shift k k)))))

(display 'c)

(display (f 42))
