(require racket/control)

(define f
  (reset (+
    (begin (print 'a) 1)
    (begin (print 'b) (shift k k)))))

(print 'c)

(print (f 42))
