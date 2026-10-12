(import (scheme base) (scheme eval) (scheme repl) (scheme write))

(define e (interaction-environment))

(eval '(define x 42) e)

(display (eval 'x e))
