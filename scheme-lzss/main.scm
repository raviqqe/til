(import (scheme base) (scheme write))

(define (debug . xs)
  (write xs (current-error-port))
  (newline (current-error-port)))

; Compression

; TODO
; (define maximum-window-size 128) ; inclusive
(define minimum-match 2) ; exclusive
; TODO
; (define maximum-match 255) ; inclusive

; TODO
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
  (make-compressor buffer current last length)
  compressor?
  (buffer compressor-buffer compressor-set-buffer!)
  (current compressor-current compressor-set-current!)
  (last compressor-last compressor-set-last!)
  (length compressor-length compressor-set-length!))

(define (compressor-push! compressor x)
  (let ((xs (list x)))
    (if (pair? (compressor-buffer compressor))
      (set-cdr! (compressor-last compressor) xs)
      (begin
        (compressor-set-buffer! compressor xs)
        (compressor-set-current! compressor xs)))
    (compressor-set-last! compressor xs)
    (compressor-set-length!
      compressor
      (+ (compressor-length compressor) 1))))

; TODO
(define (compressor-pop! compressor)
  (let ((xs (compressor-buffer compressor)))
    (compressor-set-buffer! compressor (cdr xs))
    (car xs)))

; TODO
(define (compressor-skip! compressor n)
  (compressor-set-buffer!
    compressor
    (list-tail (compressor-buffer compressor) n)))

(define (compressor-write-next compressor)
  ; TODO
  (let-values (((i n) (values 0 0)))
    (if (> n minimum-match)
      (begin
        (write-u8 (+ 1 (* 2 i)))
        (write-u8 n)
        (compressor-skip! compressor n))
      (write-u8 (* 2 (compressor-pop! compressor))))))

(define (compressor-write compressor x)
  (compressor-push! compressor x)

  ; TODO
  ; (compressor-write-next compressor)

  #f)

(define (compressor-flush compressor)
  (do ()
    ((null? (compressor-buffer compressor)))
    (compressor-write-next compressor)))

; Main

(define (compress xs expected)
  (let* ((compressor (make-compressor '() '() #f 0))
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
