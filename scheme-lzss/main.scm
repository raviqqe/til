(import (scheme base) (scheme write))

(define (debug . xs)
  (write xs (current-error-port))
  (newline (current-error-port)))

; Compression

(define maximum-window-size 128) ; inclusive
(define minimum-match 2) ; exclusive
(define maximum-match 255) ; inclusive

(define (list-maybe-ref xs index)
  (cond
    ((not (pair? xs))
      #f)
    ((zero? index)
      (car xs))
    (else
      (list-maybe-ref (cdr xs) (- index 1)))))

;; Compressor

(define-record-type compressor
  (make-compressor buffer current last back ahead)
  compressor?
  (buffer compressor-buffer compressor-set-buffer!)
  (current compressor-current compressor-set-current!)
  (last compressor-last compressor-set-last!)
  (back compressor-back compressor-set-back!)
  (ahead compressor-ahead compressor-set-ahead!))

(define (compressor-ref compressor i)
  (list-maybe-ref (compressor-buffer compressor) i))

(define (compressor-push! compressor x)
  (let ((xs (list x)))
    (if (pair? (compressor-buffer compressor))
      (set-cdr! (compressor-last compressor) xs)
      (begin
        (compressor-set-buffer! compressor xs)
        (compressor-set-current! compressor xs)))
    (compressor-set-last! compressor xs)
    (compressor-set-ahead!
      compressor
      (+ (compressor-ahead compressor) 1))))

(define (compressor-pop! compressor n)
  (let ((xs (compressor-current compressor)))
    (compressor-set-current! compressor (list-tail xs n))
    (compressor-set-ahead! compressor (- (compressor-ahead compressor) n))
    (compressor-set-back! compressor (+ (compressor-back compressor) n))

    (let ((d (- (compressor-back compressor) maximum-window-size)))
      (when (positive? d)
        (compressor-set-buffer!
          compressor
          (list-tail (compressor-buffer compressor) d))
        (compressor-set-back! compressor (- (compressor-back compressor) d))))

    (car xs)))

(define (compressor-write-next compressor)
  (let-values (((i n)
                 (let loop ((i (compressor-back compressor)) (j 0) (n 0))
                   (if (zero? i)
                     (values j n)
                     (let ((m
                             (let loop ((n 0))
                               (if (and
                                    (< n maximum-match)
                                    (eq?
                                      (compressor-ref
                                        compressor
                                        (+ (compressor-back compressor) n))
                                      (compressor-ref
                                        compressor
                                        (- (+ (compressor-back compressor) n) i))))
                                 (loop (+ n 1))
                                 n))))
                       (apply
                         loop
                         (- i 1)
                         (if (> m n)
                           (list i m)
                           (list j n))))))))
    (if (> n minimum-match)
      (begin
        (write-u8 (+ 1 (* 2 i)))
        (write-u8 n)
        (compressor-pop! compressor n))
      (write-u8 (* 2 (compressor-pop! compressor 1))))))

(define (compressor-write compressor x)
  (compressor-push! compressor x)

  (when (> (compressor-back compressor) maximum-match)
    (compressor-write-next compressor)))

(define (compressor-flush compressor)
  (do ()
    ((null? (compressor-current compressor)))
    (compressor-write-next compressor)))

; Main

(define (compress xs expected)
  (let* ((compressor (make-compressor '() '() #f 0 0))
         (ys
           (parameterize ((current-output-port (open-output-bytevector)))
             (for-each
               (lambda (x)
                 (compressor-write compressor x))
               xs)
             (compressor-flush compressor)
             (get-output-bytevector (current-output-port)))))
    (display (if (equal? ys expected) "OK" "FAIL") (current-error-port))
    (display "\t" (current-error-port))
    (debug xs ys expected)))

(for-each
  (lambda (pair)
    (compress (car pair) (cdr pair)))
  '(((11) . #u8(22))
    ((11 22) . #u8(22 44))
    ((11 22 33) . #u8(22 44 66))
    ((11 22 33 44) . #u8(22 44 66 88))
    ((11 22 33 44 55) . #u8(22 44 66 88 110))
    ((11 11 11 11) . #u8(22 1 3))))
