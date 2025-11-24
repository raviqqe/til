(import (scheme base) (scheme write))

(define (debug . xs)
  (write xs (current-error-port))
  (newline (current-error-port)))

; Compression

(define maximum-window-size 128) ; inclusive
(define minimum-match 2) ; exclusive
(define maximum-match 255) ; inclusive

;; Compressor

(define-record-type compressor
  (make-compressor buffer last)
  compressor?
  (buffer compressor-buffer compressor-set-buffer!)
  (current compressor-current compressor-set-current!)
  (last compressor-last compressor-set-last!))

(define (compressor-ref compressor index)
  (list-maybe-ref (compressor-buffer compressor) index))

(define (compressor-pop! compressor)
  (let ((xs (compressor-buffer compressor)))
    (compressor-set-buffer! compressor (cdr xs))
    (car xs)))

(define (compressor-push! compressor x)
  (let ((xs (list x)))
    (if (compressor-last compressor)
      (set-cdr! (compressor-last compressor) xs)
      (compressor-set-buffer! compressor xs))
    (compressor-set-last! compressor xs)))

(define (compressor-skip! compressor n)
  (compressor-set-buffer!
    compressor
    (list-tail (compressor-buffer compressor) n)))

(define (compressor-write-next compressor)
  (define buffer (compressor-buffer compressor))

  (let-values
    (((i n)
        (let loop ((xs (compressor-buffer compressor) (j 0) (n 0)))
          (if (eq? xs (compressor-current compressor))
            (values j n)
            (let ((m
                    (let loop ((n 0))
                      (if (and
                           (< n maximum-match)
                           (eq?
                             (compressor-ref compressor n)
                             (compressor-ref compressor (- n i 1))))
                        (loop (+ n 1))
                        n))))
              (apply
                loop
                (+ i 1)
                (if (> m n)
                  (list i m)
                  (list j n))))))))
    (if (> n minimum-match)
      (begin
        (write-u8 (+ 1 (* 2 i)))
        (write-u8 n)
        (buffer-skip! buffer n))
      (write-u8 (* 2 (buffer-pop! buffer))))))

(define (compressor-write compressor x)
  (compressor-push! compressor x)

  ; TODO
  ; (compressor-write-next compressor)
  #f)

(define (compressor-flush compressor)
  (do ()
    ((null? (compressor-current compressor)))
    (compressor-write-next compressor)))

; Main

(define (compress xs expected)
  (let* ((compressor (make-compressor '() #f #f))
         (ys
           (parameterize ((current-output-port (open-output-bytevector)))
             (for-each
               (lambda (x)
                 (compressor-write compressor x))
               xs)
             (compressor-flush compressor)
             (get-output-bytevector (current-output-port)))))
    (debug 'TEST xs (equal? ys expected) ys expected)))

(for-each
  (lambda (pair)
    (compress (car pair) (cdr pair)))
  '(((11) . #u8(22))
    ((11 22) . #u8(22 44))
    ((11 22 33) . #u8(22 44 66))
    ((11 22 33 44) . #u8(22 44 66 88))
    ((11 22 33 44 55) . #u8(22 44 66 88 110))))
