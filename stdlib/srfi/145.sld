(define-library (srfi 145)
  (import (scheme base))
  (export assume)
  (begin
    (define-syntax assume
      (syntax-rules ()
        ((assume expression message ...)
         (or expression
             (error "invalid assumption" (quote expression) message ...)))
        ((assume . _)
         (syntax-error "invalid assume syntax"))))))
