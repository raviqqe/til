(import (scheme base) (scheme eval) (scheme write))

(define e (environment '(scheme base)))

(eval '(define x 42) e)

(display (eval 'x e))
