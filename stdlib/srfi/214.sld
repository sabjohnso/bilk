(define-library (srfi 214)
  (import (scheme base) (scheme case-lambda))
  (export
    flexvector? flexvector-length flexvector-ref flexvector-set!
    make-flexvector flexvector
    flexvector-add! flexvector-add-front! flexvector-add-back!
    flexvector-remove! flexvector-remove-front! flexvector-remove-back!
    flexvector-empty? flexvector->vector vector->flexvector
    flexvector->list list->flexvector flexvector-copy
    flexvector-append flexvector-append!
    flexvector-map flexvector-map! flexvector-for-each
    flexvector-filter flexvector-filter!
    flexvector-fold flexvector-fold-right
    flexvector-count flexvector-index flexvector-any flexvector-every
    flexvector-clear! flexvector-fill! flexvector-reverse!
    flexvector-swap!)
  (begin
    (define-record-type <flexvector>
      (%make-fv storage len)
      flexvector?
      (storage fv-storage set-fv-storage!)
      (len fv-len set-fv-len!))

    (define make-flexvector
      (case-lambda
        ((size) (make-flexvector size 0))
        ((size fill)
         (let ((fv (%make-fv (make-vector (max size 4) fill) size)))
           fv))))

    (define (flexvector . elts)
      (list->flexvector elts))

    (define (flexvector-length fv) (fv-len fv))

    (define (flexvector-ref fv i)
      (if (or (< i 0) (>= i (fv-len fv)))
          (error "flexvector-ref: index out of range" i)
          (vector-ref (fv-storage fv) i)))

    (define (flexvector-set! fv i val)
      (if (or (< i 0) (>= i (fv-len fv)))
          (error "flexvector-set!: index out of range" i)
          (vector-set! (fv-storage fv) i val)))

    (define (fv-grow! fv needed)
      (let ((stor (fv-storage fv)))
        (when (> needed (vector-length stor))
          (let* ((new-cap (max needed (* 2 (vector-length stor))))
                 (new-stor (make-vector new-cap)))
            (let loop ((i 0))
              (when (< i (fv-len fv))
                (vector-set! new-stor i (vector-ref stor i))
                (loop (+ i 1))))
            (set-fv-storage! fv new-stor)))))

    (define (flexvector-add! fv i . vals)
      (let ((n (length vals)))
        (fv-grow! fv (+ (fv-len fv) n))
        ;; Shift elements right
        (let ((stor (fv-storage fv)))
          (let loop ((j (- (+ (fv-len fv) n) 1)))
            (when (>= j (+ i n))
              (vector-set! stor j (vector-ref stor (- j n)))
              (loop (- j 1))))
          ;; Insert
          (let loop ((j i) (vs vals))
            (unless (null? vs)
              (vector-set! stor j (car vs))
              (loop (+ j 1) (cdr vs))))
          (set-fv-len! fv (+ (fv-len fv) n)))))

    (define (flexvector-add-front! fv . vals)
      (apply flexvector-add! fv 0 vals))

    (define (flexvector-add-back! fv . vals)
      (apply flexvector-add! fv (fv-len fv) vals))

    (define (flexvector-remove! fv i)
      (let ((val (flexvector-ref fv i))
            (stor (fv-storage fv)))
        (let loop ((j i))
          (when (< j (- (fv-len fv) 1))
            (vector-set! stor j (vector-ref stor (+ j 1)))
            (loop (+ j 1))))
        (set-fv-len! fv (- (fv-len fv) 1))
        val))

    (define (flexvector-remove-front! fv) (flexvector-remove! fv 0))
    (define (flexvector-remove-back! fv) (flexvector-remove! fv (- (fv-len fv) 1)))

    (define (flexvector-empty? fv) (= (fv-len fv) 0))

    (define (flexvector->vector fv)
      (let ((v (make-vector (fv-len fv))))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (vector-set! v i (vector-ref (fv-storage fv) i))
            (loop (+ i 1))))
        v))

    (define (vector->flexvector vec)
      (let* ((len (vector-length vec))
             (fv (%make-fv (make-vector (max len 4)) len)))
        (let loop ((i 0))
          (when (< i len)
            (vector-set! (fv-storage fv) i (vector-ref vec i))
            (loop (+ i 1))))
        fv))

    (define (flexvector->list fv)
      (let loop ((i (- (fv-len fv) 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (vector-ref (fv-storage fv) i) acc)))))

    (define (list->flexvector lst)
      (let ((fv (make-flexvector 0)))
        (for-each (lambda (x) (flexvector-add-back! fv x)) lst)
        fv))

    (define (flexvector-copy fv)
      (let ((nfv (%make-fv (vector-copy (fv-storage fv)) (fv-len fv))))
        nfv))

    (define (flexvector-append . fvs)
      (let ((result (make-flexvector 0)))
        (for-each (lambda (fv)
          (let loop ((i 0))
            (when (< i (fv-len fv))
              (flexvector-add-back! result (vector-ref (fv-storage fv) i))
              (loop (+ i 1)))))
          fvs)
        result))

    (define (flexvector-append! fv . fvs)
      (for-each (lambda (fv2)
        (let loop ((i 0))
          (when (< i (fv-len fv2))
            (flexvector-add-back! fv (vector-ref (fv-storage fv2) i))
            (loop (+ i 1)))))
        fvs)
      fv)

    (define (flexvector-map proc fv)
      (let ((result (make-flexvector (fv-len fv))))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (flexvector-set! result i (proc (vector-ref (fv-storage fv) i)))
            (loop (+ i 1))))
        result))

    (define (flexvector-map! proc fv)
      (let loop ((i 0))
        (when (< i (fv-len fv))
          (vector-set! (fv-storage fv) i (proc (vector-ref (fv-storage fv) i)))
          (loop (+ i 1)))))

    (define (flexvector-for-each proc fv)
      (let loop ((i 0))
        (when (< i (fv-len fv))
          (proc (vector-ref (fv-storage fv) i))
          (loop (+ i 1)))))

    (define (flexvector-filter pred fv)
      (let ((result (make-flexvector 0)))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (let ((v (vector-ref (fv-storage fv) i)))
              (when (pred v) (flexvector-add-back! result v)))
            (loop (+ i 1))))
        result))

    (define (flexvector-filter! pred fv)
      (let ((j 0))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (let ((v (vector-ref (fv-storage fv) i)))
              (when (pred v)
                (vector-set! (fv-storage fv) j v)
                (set! j (+ j 1))))
            (loop (+ i 1))))
        (set-fv-len! fv j)))

    (define (flexvector-fold proc seed fv)
      (let loop ((i 0) (acc seed))
        (if (>= i (fv-len fv)) acc
            (loop (+ i 1) (proc acc (vector-ref (fv-storage fv) i))))))

    (define (flexvector-fold-right proc seed fv)
      (let loop ((i (- (fv-len fv) 1)) (acc seed))
        (if (< i 0) acc
            (loop (- i 1) (proc acc (vector-ref (fv-storage fv) i))))))

    (define (flexvector-count pred fv)
      (flexvector-fold (lambda (n x) (if (pred x) (+ n 1) n)) 0 fv))

    (define (flexvector-index pred fv)
      (let loop ((i 0))
        (cond ((>= i (fv-len fv)) #f)
              ((pred (vector-ref (fv-storage fv) i)) i)
              (else (loop (+ i 1))))))

    (define (flexvector-any pred fv)
      (let loop ((i 0))
        (if (>= i (fv-len fv)) #f
            (let ((v (pred (vector-ref (fv-storage fv) i))))
              (if v v (loop (+ i 1)))))))

    (define (flexvector-every pred fv)
      (let loop ((i 0) (last #t))
        (if (>= i (fv-len fv)) last
            (let ((v (pred (vector-ref (fv-storage fv) i))))
              (if v (loop (+ i 1) v) #f)))))

    (define (flexvector-clear! fv)
      (set-fv-len! fv 0))

    (define (flexvector-fill! fv val)
      (let loop ((i 0))
        (when (< i (fv-len fv))
          (vector-set! (fv-storage fv) i val)
          (loop (+ i 1)))))

    (define (flexvector-reverse! fv)
      (let loop ((lo 0) (hi (- (fv-len fv) 1)))
        (when (< lo hi)
          (let ((tmp (vector-ref (fv-storage fv) lo)))
            (vector-set! (fv-storage fv) lo (vector-ref (fv-storage fv) hi))
            (vector-set! (fv-storage fv) hi tmp))
          (loop (+ lo 1) (- hi 1)))))

    (define (flexvector-swap! fv i j)
      (let ((tmp (flexvector-ref fv i)))
        (flexvector-set! fv i (flexvector-ref fv j))
        (flexvector-set! fv j tmp)))))
