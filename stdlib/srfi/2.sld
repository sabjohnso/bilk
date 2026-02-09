(define-library (srfi 2)
  (import (scheme base))
  (export and-let*)
  (begin
    (define-syntax and-let*
      (syntax-rules ()
        ((and-let* () body ...)
         (begin #t body ...))
        ((and-let* ((name expr) rest ...) body ...)
         (let ((name expr))
           (if name (and-let* (rest ...) body ...) #f)))
        ((and-let* ((test) rest ...) body ...)
         (let ((t test))
           (if t (and-let* (rest ...) body ...) #f)))))))
