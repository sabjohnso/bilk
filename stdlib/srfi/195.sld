(define-library (srfi 195)
  (import (scheme base))
  (export box box? unbox set-box! box-arity unbox-value set-box-value!)
  (begin
    (define-record-type <mv-box>
      (%make-box vals)
      box?
      (vals %box-vals %set-box-vals!))

    (define (box . vals) (%make-box (list->vector vals)))

    (define (unbox b) (apply values (vector->list (%box-vals b))))

    (define (set-box! b . vals) (%set-box-vals! b (list->vector vals)))

    (define (box-arity b) (vector-length (%box-vals b)))

    (define (unbox-value b i)
      (vector-ref (%box-vals b) i))

    (define (set-box-value! b i v)
      (vector-set! (%box-vals b) i v))))
