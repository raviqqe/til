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

;; Buffer

(define-record-type buffer
  (make-buffer values last)
  buffer?
  (values buffer-values buffer-set-values!)
  (last buffer-last buffer-set-last!))

(define (buffer-ref buffer index)
  (list-maybe-ref (buffer-values buffer) index))

(define (buffer-pop! buffer)
  (let ((xs (buffer-values buffer)))
    (buffer-set-values! buffer (cdr xs))
    (car xs)))

(define (buffer-push! buffer x)
  (let ((xs (list x)))
    (if (buffer-last buffer)
      (set-cdr! (buffer-last buffer) xs)
      (buffer-set-values! buffer xs))
    (buffer-set-last! buffer xs)))

(define (buffer-skip! buffer n)
  (buffer-set-values!
    buffer
    (list-tail (buffer-values buffer) n)))

;; Window

(define-record-type window
  (make-window values length)
  window?
  (values window-values window-set-values!)
  (length window-length window-set-length!))

(define (window-ref window index)
  (list-maybe-ref (window-values window) index))

(define (window-push! window x)
  (let ((xs (window-values window))
        (n (window-length window)))
    (window-set-values! window (cons x xs))
    (if (< n (* 2 maximum-window-size))
      (window-set-length! window (+ 1 n))
      (begin
        (set-cdr! (list-tail xs (- maximum-window-size 2)) '())
        (window-set-length! window maximum-window-size)))))

;; Compressor

(define-record-type compressor
  (make-compressor buffer window)
  compressor?
  (buffer compressor-buffer)
  (window compressor-window))

(define (compressor-ref compressor i)
  (if (negative? i)
    (window-ref (compressor-window compressor) (- (+ i 1)))
    (buffer-ref (compressor-buffer compressor) i)))

(define (compressor-write-next compressor)
  (define buffer (compressor-buffer compressor))
  (define window (compressor-window compressor))

  (let ((xs (buffer-values buffer)))
    (let-values (((i n)
                   (let loop ((i 0) (j 0) (n 0))
                     (if (< i (min maximum-window-size (window-length window)))
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
                             (list j n))))
                       (values j n)))))
      (if (> n minimum-match)
        (begin
          (write-u8 (+ 1 (* 2 i)))
          (write-u8 n)
          (buffer-skip! buffer n))
        (write-u8 (* 2 (buffer-pop! buffer)))))))

(define (compressor-write compressor x)
  (define buffer (compressor-buffer compressor))
  (define window (compressor-window compressor))

  (buffer-push! buffer x)
  (window-push! window x)

  (unless (< (window-length window) maximum-window-size)
    (compressor-write-next compressor)))

(define (compressor-flush compressor)
  (do ()
    ((null? (buffer-values (compressor-buffer compressor))))
    (compressor-write-next compressor)))

; Main

(define (compress xs expected)
  (let* ((compressor
           (make-compressor
             (make-buffer '() #f)
             (make-window '() 0)))
         (ys
           (parameterize ((current-output-port (open-output-bytevector)))
             (for-each
               (lambda (x)
                 (compressor-write compressor x))
               xs)
             (compressor-flush compressor)
             (get-output-bytevector (current-output-port)))))
    (debug xs (equal? ys expected) ys expected)))

(for-each
  (lambda (pair)
    (compress (car pair) (cdr pair)))
  '(((11) . #u8(22))
    ((11 22) . #u8(22 44))
    ((11 22 33) . #u8(22 44 66))
    ((11 22 33 44) . #u8(22 44 66 88))
    ((11 22 33 44 55) . #u8(22 44 66 88 110))))
