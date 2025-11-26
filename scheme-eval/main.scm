(import (scheme base) (scheme eval))

(define y '(define x 42))

(eval y (environment '(scheme base)))

(display x)
