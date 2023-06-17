(define (f x)
	(+ x x))

(define (i x) x)

(defmacro (m x) x)

(print (m (f 5)))
