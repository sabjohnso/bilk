(define-library (srfi 210)
  (import (scheme base) (scheme case-lambda) (srfi 195))
  (export
    apply/mv call/mv list/mv vector/mv box/mv value/mv
    coarity with-values bind/mv
    list-values vector-values box-values value
    identity compose-left compose-right map-values
    bind/list bind/box bind
    set!-values)
  (begin
    (define (identity . args) (apply values args))

    (define (value . args)
      (if (= (length args) 1) (car args)
          (apply values args)))

    (define (list-values lst) (apply values lst))
    (define (vector-values vec) (apply values (vector->list vec)))
    (define (box-values b) (unbox b))

    (define (coarity thunk)
      (call-with-values thunk (lambda args (length args))))

    (define (list/mv . args)
      (let loop ((as args))
        (if (null? (cdr as))
            (call-with-values (lambda () (car as)) list)
            (cons (car as) (loop (cdr as))))))

    (define (vector/mv . args)
      (list->vector (apply list/mv args)))

    (define (box/mv . args)
      (apply box (apply list/mv args)))

    (define (value/mv . args)
      (apply values (apply list/mv args)))

    (define (apply/mv proc . args)
      (apply proc (apply list/mv args)))

    (define (call/mv consumer . producers)
      (apply consumer
        (apply append (map (lambda (p)
          (call-with-values p list)) producers))))

    (define (with-values producer consumer)
      (call-with-values producer consumer))

    (define (bind/mv producer . consumers)
      (let loop ((vals (call-with-values producer list))
                 (cs consumers))
        (if (null? cs)
            (apply values vals)
            (loop (call-with-values (lambda () (apply (car cs) vals)) list)
                  (cdr cs)))))

    (define compose-left
      (case-lambda
        (() identity)
        ((f) f)
        ((f . rest)
         (lambda args
           (let loop ((vals (call-with-values (lambda () (apply f args)) list))
                      (ps rest))
             (if (null? ps)
                 (apply values vals)
                 (loop (call-with-values (lambda () (apply (car ps) vals)) list)
                       (cdr ps))))))))

    (define compose-right
      (case-lambda
        (() identity)
        ((f) f)
        ((f . rest)
         (let ((chain (apply compose-left (reverse (cons f rest)))))
           chain))))

    (define (map-values proc)
      (lambda args (apply values (map proc args))))

    (define (bind/list proc lst) (apply proc lst))

    (define (bind/box proc b)
      (call-with-values (lambda () (unbox b)) proc))

    (define (bind proc . args)
      (apply proc args))

    ;; set!-values cannot use set! in syntax-rules templates due to
    ;; hygiene limitations.  Use let-values + begin workaround.
    (define-syntax set!-values
      (syntax-rules ()
        ((_ () expr) (begin expr (if #f #f)))
        ((_ (v1) expr)
         (let-values (((t1) expr))
           (%set!-1 v1 t1)))
        ((_ (v1 v2) expr)
         (let-values (((t1 t2) expr))
           (%set!-1 v1 t1)
           (%set!-1 v2 t2)))
        ((_ (v1 v2 v3) expr)
         (let-values (((t1 t2 t3) expr))
           (%set!-1 v1 t1)
           (%set!-1 v2 t2)
           (%set!-1 v3 t3)))))
    (define-syntax %set!-1
      (syntax-rules ()
        ((_ var val) (set! var val)))))))
