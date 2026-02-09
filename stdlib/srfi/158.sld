(define-library (srfi 158)
  (import (scheme base) (scheme case-lambda))
  (export
    generator circular-generator make-iota-generator make-range-generator
    make-coroutine-generator list->generator vector->generator
    reverse-vector->generator string->generator bytevector->generator
    make-for-each-generator make-unfold-generator
    gcons* gappend gcombine gfilter gremove gtake gdrop
    gtake-while gdrop-while gflatten ggroup gmerge gmap
    gstate-filter gdelete gdelete-neighbor-dups gindex gselect
    generator->list generator->reverse-list generator->vector
    generator->vector! generator->string
    generator-fold generator-map->list generator-for-each
    generator-find generator-count generator-any generator-every
    generator-unfold
    make-accumulator count-accumulator list-accumulator
    reverse-list-accumulator vector-accumulator
    reverse-vector-accumulator vector-accumulator!
    string-accumulator bytevector-accumulator bytevector-accumulator!
    sum-accumulator product-accumulator)
  (begin
    (define (generator . vals)
      (let ((lst vals))
        (lambda ()
          (if (null? lst) (eof-object)
              (let ((v (car lst)))
                (set! lst (cdr lst))
                v)))))

    (define (circular-generator . vals)
      (let ((lst vals) (cur vals))
        (lambda ()
          (when (null? cur) (set! cur lst))
          (let ((v (car cur)))
            (set! cur (cdr cur))
            v))))

    (define make-iota-generator
      (case-lambda
        ((count) (make-iota-generator count 0 1))
        ((count start) (make-iota-generator count start 1))
        ((count start step)
         (let ((i 0))
           (lambda ()
             (if (>= i count) (eof-object)
                 (let ((v (+ start (* i step))))
                   (set! i (+ i 1))
                   v)))))))

    (define make-range-generator
      (case-lambda
        ((start) (let ((n start))
                   (lambda () (let ((v n)) (set! n (+ n 1)) v))))
        ((start end) (make-range-generator start end 1))
        ((start end step)
         (let ((n start))
           (lambda ()
             (if (if (> step 0) (>= n end) (<= n end))
                 (eof-object)
                 (let ((v n)) (set! n (+ n step)) v)))))))

    (define (make-coroutine-generator proc)
      (define return #f)
      (define resume #f)
      (define (yield v)
        (call-with-current-continuation
          (lambda (k)
            (set! resume k)
            (return v))))
      (set! resume
        (lambda (v)
          (proc yield)
          (return (eof-object))))
      (lambda ()
        (call-with-current-continuation
          (lambda (k)
            (set! return k)
            (resume #f)))))

    (define (list->generator lst)
      (let ((cur lst))
        (lambda ()
          (if (null? cur) (eof-object)
              (let ((v (car cur)))
                (set! cur (cdr cur))
                v)))))

    (define vector->generator
      (case-lambda
        ((vec) (vector->generator vec 0 (vector-length vec)))
        ((vec start) (vector->generator vec start (vector-length vec)))
        ((vec start end)
         (let ((i start))
           (lambda ()
             (if (>= i end) (eof-object)
                 (let ((v (vector-ref vec i)))
                   (set! i (+ i 1))
                   v)))))))

    (define reverse-vector->generator
      (case-lambda
        ((vec) (reverse-vector->generator vec 0 (vector-length vec)))
        ((vec start) (reverse-vector->generator vec start (vector-length vec)))
        ((vec start end)
         (let ((i (- end 1)))
           (lambda ()
             (if (< i start) (eof-object)
                 (let ((v (vector-ref vec i)))
                   (set! i (- i 1))
                   v)))))))

    (define string->generator
      (case-lambda
        ((str) (string->generator str 0 (string-length str)))
        ((str start) (string->generator str start (string-length str)))
        ((str start end)
         (let ((i start))
           (lambda ()
             (if (>= i end) (eof-object)
                 (let ((v (string-ref str i)))
                   (set! i (+ i 1))
                   v)))))))

    (define (bytevector->generator bv)
      (let ((i 0) (len (bytevector-length bv)))
        (lambda ()
          (if (>= i len) (eof-object)
              (let ((v (bytevector-u8-ref bv i)))
                (set! i (+ i 1))
                v)))))

    (define (make-for-each-generator for-each obj)
      (make-coroutine-generator
        (lambda (yield) (for-each yield obj))))

    (define (make-unfold-generator stop? mapper successor seed)
      (let ((s seed) (done #f))
        (lambda ()
          (if done (eof-object)
              (if (stop? s) (begin (set! done #t) (eof-object))
                  (let ((v (mapper s)))
                    (set! s (successor s))
                    v))))))

    (define (gcons* . args)
      (let ((cur args))
        (lambda ()
          (if (null? (cdr cur))
              ((car cur))
              (let ((v (car cur)))
                (set! cur (cdr cur))
                v)))))

    (define (gappend . gens)
      (let ((gs gens))
        (lambda ()
          (let loop ()
            (if (null? gs) (eof-object)
                (let ((v ((car gs))))
                  (if (eof-object? v)
                      (begin (set! gs (cdr gs)) (loop))
                      v)))))))

    (define (gcombine proc seed . gens)
      (let ((s seed))
        (lambda ()
          (let ((vals (map (lambda (g) (g)) gens)))
            (if (any-eof? vals) (eof-object)
                (call-with-values (lambda () (apply proc (append vals (list s))))
                  (lambda (val . new-seed)
                    (set! s (car new-seed))
                    val)))))))

    (define (any-eof? lst)
      (cond ((null? lst) #f)
            ((eof-object? (car lst)) #t)
            (else (any-eof? (cdr lst)))))

    (define (gfilter pred gen)
      (lambda ()
        (let loop ()
          (let ((v (gen)))
            (cond ((eof-object? v) v)
                  ((pred v) v)
                  (else (loop)))))))

    (define (gremove pred gen)
      (gfilter (lambda (x) (not (pred x))) gen))

    (define gtake
      (case-lambda
        ((gen k) (gtake gen k (eof-object)))
        ((gen k padding)
         (let ((i 0))
           (lambda ()
             (if (>= i k) (eof-object)
                 (begin (set! i (+ i 1))
                        (let ((v (gen)))
                          (if (eof-object? v) padding v)))))))))

    (define (gdrop gen k)
      (let ((i 0))
        (lambda ()
          (let loop ()
            (if (< i k)
                (begin (set! i (+ i 1)) (gen) (loop))
                (gen))))))

    (define (gtake-while pred gen)
      (let ((done #f))
        (lambda ()
          (if done (eof-object)
              (let ((v (gen)))
                (if (or (eof-object? v) (not (pred v)))
                    (begin (set! done #t) (eof-object))
                    v))))))

    (define (gdrop-while pred gen)
      (let ((dropping #t))
        (lambda ()
          (let loop ()
            (let ((v (gen)))
              (cond ((eof-object? v) v)
                    ((and dropping (pred v)) (loop))
                    (else (set! dropping #f) v)))))))

    (define (gflatten gen)
      (let ((cur '()))
        (lambda ()
          (let loop ()
            (if (pair? cur)
                (let ((v (car cur)))
                  (set! cur (cdr cur))
                  v)
                (let ((v (gen)))
                  (if (eof-object? v) v
                      (begin (set! cur v) (loop)))))))))

    (define ggroup
      (case-lambda
        ((gen k) (ggroup gen k #f))
        ((gen k padding)
         (let ((done #f))
           (lambda ()
             (if done (eof-object)
                 (let loop ((i 0) (acc '()))
                   (if (>= i k)
                       (reverse acc)
                       (let ((v (gen)))
                         (if (eof-object? v)
                             (if (null? acc)
                                 (begin (set! done #t) (eof-object))
                                 (begin (set! done #t)
                                        (if padding
                                            (let pad ((j i) (a acc))
                                              (if (>= j k) (reverse a)
                                                  (pad (+ j 1) (cons padding a))))
                                            (reverse acc))))
                             (loop (+ i 1) (cons v acc))))))))))))

    (define (gmerge less? gen1 gen2)
      (let ((v1 #f) (v2 #f) (need1 #t) (need2 #t))
        (lambda ()
          (when need1 (set! v1 (gen1)) (set! need1 #f))
          (when need2 (set! v2 (gen2)) (set! need2 #f))
          (cond
            ((and (eof-object? v1) (eof-object? v2)) (eof-object))
            ((eof-object? v1) (set! need2 #t) v2)
            ((eof-object? v2) (set! need1 #t) v1)
            ((less? v2 v1) (set! need2 #t) v2)
            (else (set! need1 #t) v1)))))

    (define (gmap proc . gens)
      (lambda ()
        (let ((vals (map (lambda (g) (g)) gens)))
          (if (any-eof? vals) (eof-object)
              (apply proc vals)))))

    (define (gstate-filter proc seed gen)
      (let ((s seed))
        (lambda ()
          (let loop ()
            (let ((v (gen)))
              (if (eof-object? v) v
                  (call-with-values (lambda () (proc v s))
                    (lambda (keep new-seed)
                      (set! s new-seed)
                      (if keep v (loop))))))))))

    (define gdelete
      (case-lambda
        ((item gen) (gdelete item gen equal?))
        ((item gen =)
         (gfilter (lambda (x) (not (= x item))) gen))))

    (define gdelete-neighbor-dups
      (case-lambda
        ((gen) (gdelete-neighbor-dups gen equal?))
        ((gen =)
         (let ((prev (cons #f #f)))
           (lambda ()
             (let loop ()
               (let ((v (gen)))
                 (cond ((eof-object? v) v)
                       ((and (pair? prev) (= (car prev) v)) (loop))
                       (else (set-car! prev v) v)))))))))

    (define (gindex gen index-gen)
      (let ((i 0))
        (lambda ()
          (let ((idx (index-gen)))
            (if (eof-object? idx) (eof-object)
                (let loop ()
                  (let ((v (gen)))
                    (cond ((eof-object? v) v)
                          ((= i idx) (set! i (+ i 1)) v)
                          (else (set! i (+ i 1)) (loop))))))))))

    (define (gselect gen truth-gen)
      (lambda ()
        (let loop ()
          (let ((v (gen)))
            (if (eof-object? v) v
                (let ((t (truth-gen)))
                  (if (eof-object? t) (eof-object)
                      (if t v (loop)))))))))

    (define generator->list
      (case-lambda
        ((gen) (generator->list gen +inf.0))
        ((gen k)
         (let loop ((i 0) (acc '()))
           (if (>= i k) (reverse acc)
               (let ((v (gen)))
                 (if (eof-object? v) (reverse acc)
                     (loop (+ i 1) (cons v acc)))))))))

    (define (generator->reverse-list gen)
      (let loop ((acc '()))
        (let ((v (gen)))
          (if (eof-object? v) acc
              (loop (cons v acc))))))

    (define generator->vector
      (case-lambda
        ((gen) (list->vector (generator->list gen)))
        ((gen k) (list->vector (generator->list gen k)))))

    (define (generator->vector! vec at gen)
      (let loop ((i at))
        (if (>= i (vector-length vec)) i
            (let ((v (gen)))
              (if (eof-object? v) i
                  (begin (vector-set! vec i v)
                         (loop (+ i 1))))))))

    (define (generator->string gen)
      (let ((out (open-output-string)))
        (let loop ()
          (let ((v (gen)))
            (if (eof-object? v) (get-output-string out)
                (begin (write-char v out)
                       (loop)))))))

    (define (generator-fold proc seed . gens)
      (let loop ((acc seed))
        (let ((vals (map (lambda (g) (g)) gens)))
          (if (any-eof? vals) acc
              (loop (apply proc (append vals (list acc))))))))

    (define (generator-map->list proc . gens)
      (let loop ((acc '()))
        (let ((vals (map (lambda (g) (g)) gens)))
          (if (any-eof? vals) (reverse acc)
              (loop (cons (apply proc vals) acc))))))

    (define (generator-for-each proc . gens)
      (let loop ()
        (let ((vals (map (lambda (g) (g)) gens)))
          (unless (any-eof? vals)
            (apply proc vals)
            (loop)))))

    (define (generator-find pred gen)
      (let loop ()
        (let ((v (gen)))
          (cond ((eof-object? v) #f)
                ((pred v) v)
                (else (loop))))))

    (define (generator-count pred gen)
      (let loop ((n 0))
        (let ((v (gen)))
          (if (eof-object? v) n
              (loop (if (pred v) (+ n 1) n))))))

    (define (generator-any pred gen)
      (let loop ()
        (let ((v (gen)))
          (if (eof-object? v) #f
              (let ((r (pred v)))
                (if r r (loop)))))))

    (define (generator-every pred gen)
      (let loop ((last #t))
        (let ((v (gen)))
          (if (eof-object? v) last
              (let ((r (pred v)))
                (if r (loop r) #f))))))

    (define (generator-unfold gen unfold . args)
      (apply unfold eof-object? (lambda (x) x) (lambda (x) (gen))
             (gen) args))

    ;; Accumulators
    (define (make-accumulator proc seed finalize)
      (let ((s seed))
        (lambda (v)
          (if (eof-object? v) (finalize s)
              (set! s (proc v s))))))

    (define (count-accumulator)
      (make-accumulator (lambda (v n) (+ n 1)) 0 (lambda (n) n)))

    (define (list-accumulator)
      (make-accumulator (lambda (v acc) (cons v acc)) '() reverse))

    (define (reverse-list-accumulator)
      (make-accumulator (lambda (v acc) (cons v acc)) '() (lambda (x) x)))

    (define (vector-accumulator)
      (let ((acc (list-accumulator)))
        (lambda (v)
          (if (eof-object? v) (list->vector (acc v))
              (acc v)))))

    (define (reverse-vector-accumulator)
      (let ((acc (reverse-list-accumulator)))
        (lambda (v)
          (if (eof-object? v) (list->vector (acc v))
              (acc v)))))

    (define (vector-accumulator! vec at)
      (let ((i at))
        (lambda (v)
          (if (eof-object? v) vec
              (begin (vector-set! vec i v)
                     (set! i (+ i 1)))))))

    (define (string-accumulator)
      (let ((out (open-output-string)))
        (lambda (v)
          (if (eof-object? v) (get-output-string out)
              (write-char v out)))))

    (define (bytevector-accumulator)
      (let ((acc '()))
        (lambda (v)
          (if (eof-object? v)
              (let* ((lst (reverse acc))
                     (bv (make-bytevector (length lst))))
                (let loop ((i 0) (l lst))
                  (if (null? l) bv
                      (begin (bytevector-u8-set! bv i (car l))
                             (loop (+ i 1) (cdr l))))))
              (set! acc (cons v acc))))))

    (define (bytevector-accumulator! bv at)
      (let ((i at))
        (lambda (v)
          (if (eof-object? v) bv
              (begin (bytevector-u8-set! bv i v)
                     (set! i (+ i 1)))))))

    (define (sum-accumulator)
      (make-accumulator + 0 (lambda (x) x)))

    (define (product-accumulator)
      (make-accumulator * 1 (lambda (x) x)))))
