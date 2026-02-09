(define-library (srfi 41 primitive)
  (import (scheme base) (scheme lazy))
  (export stream-null stream-cons stream? stream-null?
         stream-pair? stream-car stream-cdr stream-lambda
         make-stream-promise stream-type-promise)
  (begin
    (define-record-type <stream-type>
      (make-stream-promise promise)
      stream-type?
      (promise stream-type-promise))
    (define stream-null (make-stream-promise (delay '())))
    (define-syntax stream-cons
      (syntax-rules ()
        ((stream-cons a b)
         (make-stream-promise (delay (cons a b))))))
    (define (stream? x) (stream-type? x))
    (define (stream-null? x)
      (and (stream? x)
           (null? (force (stream-type-promise x)))))
    (define (stream-pair? x)
      (and (stream? x)
           (pair? (force (stream-type-promise x)))))
    (define (stream-car s)
      (if (stream-null? s)
          (error "stream-car: empty stream")
          (car (force (stream-type-promise s)))))
    (define (stream-cdr s)
      (if (stream-null? s)
          (error "stream-cdr: empty stream")
          (cdr (force (stream-type-promise s)))))
    (define-syntax stream-lambda
      (syntax-rules ()
        ((stream-lambda formals body ...)
         (lambda formals
           (make-stream-promise (delay (begin body ...)))))))))
