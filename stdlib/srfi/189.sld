(define-library (srfi 189)
  (import (scheme base) (scheme case-lambda))
  (export
    just nothing right left
    maybe? either? just? nothing? right? left?
    maybe-ref maybe-ref/default either-ref either-ref/default
    maybe-join either-join
    maybe-bind either-bind
    maybe-compose either-compose
    maybe-length either-length
    maybe-filter maybe-remove either-filter either-remove
    maybe-sequence either-sequence
    maybe->either either->maybe
    list->just list->right maybe->list either->list
    maybe->truth truth->maybe
    maybe->values values->maybe
    maybe-map maybe-fold maybe-unfold
    maybe-for-each either-map either-fold either-unfold
    either-for-each
    maybe-and maybe-or either-and either-or
    maybe-let* either-let*
    maybe-if
    maybe= either=
    either-swap
    exception->either
    tri-not tri=? tri-and tri-or tri-merge)
  (begin
    (define-record-type <just>
      (%raw-just objs)
      just?
      (objs just-objs))
    (define-record-type <nothing>
      (%make-nothing)
      nothing?)
    (define-record-type <left>
      (%raw-left objs)
      left?
      (objs left-objs))
    (define-record-type <right>
      (%raw-right objs)
      right?
      (objs right-objs))

    (define %nothing (%make-nothing))
    (define (nothing) %nothing)

    (define (just . objs) (%raw-just objs))
    (define (right . objs) (%raw-right objs))
    (define (left . objs) (%raw-left objs))

    (define (maybe? x) (or (just? x) (nothing? x)))
    (define (either? x) (or (left? x) (right? x)))

    (define maybe-ref
      (case-lambda
        ((m failure)
         (if (just? m)
             (apply values (just-objs m))
             (failure)))
        ((m failure success)
         (if (just? m)
             (apply success (just-objs m))
             (failure)))))

    (define (maybe-ref/default m . defaults)
      (if (just? m)
          (apply values (just-objs m))
          (apply values defaults)))

    (define either-ref
      (case-lambda
        ((e failure)
         (cond ((right? e) (apply values (right-objs e)))
               ((left? e) (apply failure (left-objs e)))
               (else (error "either-ref: not an either" e))))
        ((e failure success)
         (cond ((right? e) (apply success (right-objs e)))
               ((left? e) (apply failure (left-objs e)))
               (else (error "either-ref: not an either" e))))))

    (define (either-ref/default e . defaults)
      (if (right? e)
          (apply values (right-objs e))
          (apply values defaults)))

    (define (maybe-join m)
      (if (nothing? m) m
          (let ((inner (car (just-objs m))))
            inner)))

    (define (either-join e)
      (if (left? e) e
          (let ((inner (car (right-objs e))))
            inner)))

    (define (maybe-bind m . procs)
      (let loop ((m m) (ps procs))
        (cond ((null? ps) m)
              ((nothing? m) m)
              (else (loop (apply (car ps) (just-objs m)) (cdr ps))))))

    (define (either-bind e . procs)
      (let loop ((e e) (ps procs))
        (cond ((null? ps) e)
              ((left? e) e)
              (else (loop (apply (car ps) (right-objs e)) (cdr ps))))))

    (define (maybe-compose . procs)
      (lambda args
        (let loop ((m (apply just args)) (ps procs))
          (if (or (null? ps) (nothing? m)) m
              (loop (apply (car ps) (just-objs m)) (cdr ps))))))

    (define (either-compose . procs)
      (lambda args
        (let loop ((e (apply right args)) (ps procs))
          (if (or (null? ps) (left? e)) e
              (loop (apply (car ps) (right-objs e)) (cdr ps))))))

    (define (maybe-length m) (if (just? m) 1 0))
    (define (either-length e) (if (right? e) 1 0))

    (define (maybe-filter pred m)
      (if (nothing? m) m
          (if (apply pred (just-objs m)) m (nothing))))
    (define (maybe-remove pred m)
      (if (nothing? m) m
          (if (apply pred (just-objs m)) (nothing) m)))
    (define (either-filter pred e . rest)
      (if (left? e) e
          (if (apply pred (right-objs e)) e
              (apply left rest))))
    (define (either-remove pred e . rest)
      (if (left? e) e
          (if (apply pred (right-objs e))
              (apply left rest) e)))

    (define (maybe-sequence lst . rest)
      (let ((map-fn (if (null? rest) just (car rest))))
        (let loop ((lst lst) (acc '()))
          (cond ((null? lst) (map-fn (reverse acc)))
                ((nothing? (car lst)) (nothing))
                (else (loop (cdr lst)
                            (cons (car (just-objs (car lst))) acc)))))))

    (define (either-sequence lst . rest)
      (let ((map-fn (if (null? rest) right (car rest))))
        (let loop ((lst lst) (acc '()))
          (cond ((null? lst) (map-fn (reverse acc)))
                ((left? (car lst)) (car lst))
                (else (loop (cdr lst)
                            (cons (car (right-objs (car lst))) acc)))))))

    (define (maybe->either m . rest)
      (if (just? m) (apply right (just-objs m))
          (if (null? rest) (left 'nothing)
              (apply left rest))))

    (define (either->maybe e)
      (if (right? e) (apply just (right-objs e)) (nothing)))

    (define (list->just lst) (apply just lst))
    (define (list->right lst) (apply right lst))

    (define (maybe->list m)
      (if (nothing? m) '() (just-objs m)))

    (define (either->list e)
      (if (left? e) '() (right-objs e)))

    (define (maybe->truth m)
      (if (nothing? m) #f (car (just-objs m))))

    (define (truth->maybe x)
      (if x (just x) (nothing)))

    (define (maybe->values m)
      (if (nothing? m) (values)
          (apply values (just-objs m))))

    (define (values->maybe . objs)
      (if (null? objs) (nothing) (apply just objs)))

    (define (maybe-map proc m)
      (if (nothing? m) m
          (let ((result (apply proc (just-objs m))))
            (just result))))

    (define (maybe-fold proc seed m)
      (if (nothing? m) seed
          (apply proc seed (just-objs m))))

    (define (maybe-unfold stop? seed->maybe . rest)
      (if (stop? (if (null? rest) #f (car rest)))
          (nothing)
          (seed->maybe (if (null? rest) #f (car rest)))))

    (define (maybe-for-each proc m)
      (unless (nothing? m)
        (apply proc (just-objs m))))

    (define (either-map proc e)
      (if (left? e) e
          (let ((result (apply proc (right-objs e))))
            (right result))))

    (define (either-fold proc seed e)
      (if (left? e) seed
          (apply proc seed (right-objs e))))

    (define (either-unfold stop? seed->either . rest)
      (if (stop? (if (null? rest) #f (car rest)))
          (left 'unfold-stop)
          (seed->either (if (null? rest) #f (car rest)))))

    (define (either-for-each proc e)
      (when (right? e)
        (apply proc (right-objs e))))

    (define-syntax maybe-and
      (syntax-rules ()
        ((_) (just))
        ((_ m) m)
        ((_ m rest ...)
         (let ((t m))
           (if (just? t) (maybe-and rest ...) t)))))

    (define-syntax maybe-or
      (syntax-rules ()
        ((_) (nothing))
        ((_ m) m)
        ((_ m rest ...)
         (let ((t m))
           (if (just? t) t (maybe-or rest ...))))))

    (define-syntax either-and
      (syntax-rules ()
        ((_) (right))
        ((_ e) e)
        ((_ e rest ...)
         (let ((t e))
           (if (right? t) (either-and rest ...) t)))))

    (define-syntax either-or
      (syntax-rules ()
        ((_) (left))
        ((_ e) e)
        ((_ e rest ...)
         (let ((t e))
           (if (right? t) t (either-or rest ...))))))

    (define-syntax maybe-let*
      (syntax-rules ()
        ((_ () body ...) (begin body ...))
        ((_ ((var maybe-expr) rest ...) body ...)
         (let ((m maybe-expr))
           (if (nothing? m) m
               (let ((var (car (just-objs m))))
                 (maybe-let* (rest ...) body ...)))))
        ((_ ((maybe-expr) rest ...) body ...)
         (let ((m maybe-expr))
           (if (nothing? m) m
               (maybe-let* (rest ...) body ...))))))

    (define-syntax either-let*
      (syntax-rules ()
        ((_ () body ...) (begin body ...))
        ((_ ((var either-expr) rest ...) body ...)
         (let ((e either-expr))
           (if (left? e) e
               (let ((var (car (right-objs e))))
                 (either-let* (rest ...) body ...)))))
        ((_ ((either-expr) rest ...) body ...)
         (let ((e either-expr))
           (if (left? e) e
               (either-let* (rest ...) body ...))))))

    (define-syntax maybe-if
      (syntax-rules ()
        ((_ m just-expr nothing-expr)
         (if (just? m) just-expr nothing-expr))))

    (define (maybe= = m1 m2)
      (cond ((and (nothing? m1) (nothing? m2)) #t)
            ((or (nothing? m1) (nothing? m2)) #f)
            (else (apply = (append (just-objs m1) (just-objs m2))))))

    (define (either= = e1 e2)
      (cond ((and (left? e1) (left? e2))
             (apply = (append (left-objs e1) (left-objs e2))))
            ((and (right? e1) (right? e2))
             (apply = (append (right-objs e1) (right-objs e2))))
            (else #f)))

    (define (either-swap e)
      (cond ((right? e) (apply left (right-objs e)))
            ((left? e) (apply right (left-objs e)))
            (else (error "either-swap: not an either" e))))

    (define (exception->either pred thunk)
      (guard (exn ((pred exn) (left exn)))
        (call-with-values thunk right)))

    ;; Trivalent logic
    (define (tri-not x)
      (cond ((nothing? x) x)
            ((just? x) (if (car (just-objs x)) (just #f) (just #t)))
            (else (not x))))

    (define (tri=? m1 m2)
      (cond ((and (nothing? m1) (nothing? m2)) (just #t))
            ((or (nothing? m1) (nothing? m2)) (nothing))
            (else (just (equal? (just-objs m1) (just-objs m2))))))

    (define-syntax tri-and
      (syntax-rules ()
        ((_) (just #t))
        ((_ x) x)
        ((_ x rest ...)
         (let ((t x))
           (cond ((nothing? t) t)
                 ((and (just? t) (not (car (just-objs t)))) t)
                 (else (tri-and rest ...)))))))

    (define-syntax tri-or
      (syntax-rules ()
        ((_) (just #f))
        ((_ x) x)
        ((_ x rest ...)
         (let ((t x))
           (cond ((nothing? t) t)
                 ((and (just? t) (car (just-objs t))) t)
                 (else (tri-or rest ...)))))))

    (define (tri-merge . args)
      (let loop ((as args))
        (cond ((null? as) (nothing))
              ((nothing? (car as)) (loop (cdr as)))
              (else (car as)))))))
