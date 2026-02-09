(define-library (srfi 8)
  (import (scheme base))
  (export receive)
  (begin
    (define-syntax receive
      (syntax-rules ()
        ((receive formals expr body ...)
         (call-with-values (lambda () expr)
           (lambda formals body ...)))))))
