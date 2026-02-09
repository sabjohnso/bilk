(define-library (srfi 235)
  (import (scheme base) (scheme case-lambda))
  (export
    constantly complement swap flip on-left on-right
    conjoin disjoin each-of all-of any-of on
    left-section right-section apply-chain
    arguments-drop arguments-drop-right
    arguments-take arguments-take-right
    group-by begin-procedure if-procedure when-procedure
    unless-procedure value-procedure case-procedure
    and-procedure eager-and-procedure
    or-procedure eager-or-procedure
    funcall-procedure loop-procedure while-procedure until-procedure
    always never boolean)
  (begin
    (define (constantly . vals)
      (lambda args (apply values vals)))

    (define (complement proc)
      (lambda args (not (apply proc args))))

    (define (swap proc)
      (lambda (a b . rest) (apply proc b a rest)))

    (define (flip proc)
      (lambda args (apply proc (reverse args))))

    (define (on-left proc) (lambda (a b) (proc a)))
    (define (on-right proc) (lambda (a b) (proc b)))

    (define (conjoin . preds)
      (lambda args
        (let loop ((ps preds))
          (cond ((null? ps) #t)
                ((null? (cdr ps)) (apply (car ps) args))
                ((apply (car ps) args) (loop (cdr ps)))
                (else #f)))))

    (define (disjoin . preds)
      (lambda args
        (let loop ((ps preds))
          (cond ((null? ps) #f)
                ((null? (cdr ps)) (apply (car ps) args))
                (else (let ((v (apply (car ps) args)))
                        (if v v (loop (cdr ps)))))))))

    (define (each-of . procs)
      (lambda args
        (for-each (lambda (p) (apply p args)) procs)))

    (define (all-of pred)
      (lambda (lst)
        (let loop ((lst lst))
          (cond ((null? lst) #t)
                ((null? (cdr lst)) (pred (car lst)))
                ((pred (car lst)) (loop (cdr lst)))
                (else #f)))))

    (define (any-of pred)
      (lambda (lst)
        (let loop ((lst lst))
          (if (null? lst) #f
              (let ((v (pred (car lst))))
                (if v v (loop (cdr lst))))))))

    (define (on proc . procs)
      (lambda args
        (define (zip-apply ps as)
          (if (or (null? ps) (null? as)) '()
              (cons ((car ps) (car as))
                    (zip-apply (cdr ps) (cdr as)))))
        (apply proc (zip-apply procs args))))

    (define (left-section proc . bound-args)
      (lambda args (apply proc (append bound-args args))))

    (define (right-section proc . bound-args)
      (lambda args (apply proc (append args bound-args))))

    (define (apply-chain . procs)
      (lambda args
        (let loop ((ps (reverse procs)) (vals args))
          (if (null? ps)
              (apply values vals)
              (loop (cdr ps)
                    (call-with-values
                      (lambda () (apply (car ps) vals))
                      list))))))

    (define (arguments-drop proc n)
      (lambda args (apply proc (list-tail args n))))

    (define (arguments-drop-right proc n)
      (lambda args
        (let ((len (length args)))
          (apply proc (take-list args (- len n))))))

    (define (arguments-take proc n)
      (lambda args (apply proc (take-list args n))))

    (define (arguments-take-right proc n)
      (lambda args
        (let ((len (length args)))
          (apply proc (list-tail args (- len n))))))

    (define (take-list lst n)
      (if (= n 0) '()
          (cons (car lst) (take-list (cdr lst) (- n 1)))))

    (define (group-by key . rest)
      (let ((= (if (null? rest) equal? (car rest))))
        (lambda (lst)
          (let loop ((lst lst) (groups '()))
            (if (null? lst)
                (reverse (map (lambda (g) (reverse (cdr g))) groups))
                (let* ((elt (car lst))
                       (k (key elt))
                       (found (let scan ((gs groups))
                                (cond ((null? gs) #f)
                                      ((= (caar gs) k) (car gs))
                                      (else (scan (cdr gs)))))))
                  (if found
                      (begin (set-cdr! found (cons elt (cdr found)))
                             (loop (cdr lst) groups))
                      (loop (cdr lst) (cons (list k elt) groups)))))))))

    (define (begin-procedure . thunks)
      (let loop ((ts thunks))
        (if (null? (cdr ts))
            ((car ts))
            (begin ((car ts)) (loop (cdr ts))))))

    (define (if-procedure val then-thunk else-thunk)
      (if val (then-thunk) (else-thunk)))

    (define (when-procedure val . thunks)
      (when val (for-each (lambda (t) (t)) thunks)))

    (define (unless-procedure val . thunks)
      (unless val (for-each (lambda (t) (t)) thunks)))

    (define (value-procedure val then-proc else-thunk)
      (if val (then-proc val) (else-thunk)))

    (define (case-procedure val . clauses)
      (let loop ((cs clauses))
        (cond ((null? cs) (if #f #f))
              ((null? (cdr cs)) ((car cs) val))
              (((car cs) val) => (lambda (r) r))
              (else (loop (cdr cs))))))

    (define (and-procedure . thunks)
      (let loop ((ts thunks))
        (cond ((null? ts) #t)
              ((null? (cdr ts)) ((car ts)))
              (((car ts)) (loop (cdr ts)))
              (else #f))))

    (define (eager-and-procedure . vals)
      (let loop ((vs vals))
        (cond ((null? vs) #t)
              ((null? (cdr vs)) (car vs))
              ((car vs) (loop (cdr vs)))
              (else #f))))

    (define (or-procedure . thunks)
      (let loop ((ts thunks))
        (cond ((null? ts) #f)
              ((null? (cdr ts)) ((car ts)))
              (else (let ((v ((car ts))))
                      (if v v (loop (cdr ts))))))))

    (define (eager-or-procedure . vals)
      (let loop ((vs vals))
        (cond ((null? vs) #f)
              ((null? (cdr vs)) (car vs))
              (else (let ((v (car vs)))
                      (if v v (loop (cdr vs))))))))

    (define (funcall-procedure proc . args)
      (apply proc args))

    (define (loop-procedure thunk)
      (let loop () (thunk) (loop)))

    (define (while-procedure thunk)
      (let loop ()
        (when (thunk) (loop))))

    (define (until-procedure thunk)
      (let loop ()
        (unless (thunk) (loop))))

    (define (always . args) #t)
    (define (never . args) #f)
    (define (boolean x) (if x #t #f))))
