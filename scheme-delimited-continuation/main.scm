(require racket/control)

(define y '(define x 42))

(eval y)

(display x)
