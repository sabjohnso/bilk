(define-library (srfi 31)
  (import (scheme base))
  (export rec)
  (begin
    (define-syntax rec
      (syntax-rules ()
        ((rec name expr)
         (letrec ((name expr)) name))))))
