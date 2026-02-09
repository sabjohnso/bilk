(define-library (srfi 219)
  (import (scheme base))
  (export define-curried)
  (begin
    (define-syntax define-curried
      (syntax-rules ()
        ((define-curried ((head . inner-args) . outer-args) body ...)
         (define-curried (head . inner-args)
           (lambda outer-args body ...)))
        ((define-curried (name . args) body ...)
         (define name (lambda args body ...)))
        ((define-curried name expr)
         (define name expr))))))
