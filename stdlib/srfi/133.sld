(define-library (srfi 133)
  (import (scheme base))
  (export
    vector-unfold vector-unfold-right
    vector-copy vector-reverse-copy
    vector-append vector-concatenate
    vector-empty? vector=
    vector-fold vector-fold-right vector-count
    vector-map vector-map! vector-for-each
    vector-index vector-index-right vector-skip vector-skip-right
    vector-any vector-every
    vector-swap! vector-fill! vector-reverse!
    vector-reverse-copy! vector-copy!
    vector-partition
    vector-binary-search)
  (begin
    (define (vector-unfold f len . maybe-seeds)
      (let ((vec (make-vector len))
            (seed (if (null? maybe-seeds) #f (car maybe-seeds))))
        (if (null? maybe-seeds)
            (let loop ((i 0))
              (if (>= i len) vec
                  (begin (vector-set! vec i (f i))
                         (loop (+ i 1)))))
            (let loop ((i 0) (seed seed))
              (if (>= i len) vec
                  (call-with-values (lambda () (f i seed))
                    (lambda (val . rest)
                      (vector-set! vec i val)
                      (loop (+ i 1) (if (null? rest) seed (car rest))))))))))

    (define (vector-unfold-right f len . maybe-seeds)
      (let ((vec (make-vector len))
            (seed (if (null? maybe-seeds) #f (car maybe-seeds))))
        (if (null? maybe-seeds)
            (let loop ((i (- len 1)))
              (if (< i 0) vec
                  (begin (vector-set! vec i (f i))
                         (loop (- i 1)))))
            (let loop ((i (- len 1)) (seed seed))
              (if (< i 0) vec
                  (call-with-values (lambda () (f i seed))
                    (lambda (val . rest)
                      (vector-set! vec i val)
                      (loop (- i 1) (if (null? rest) seed (car rest))))))))))

    (define (vector-empty? vec) (= (vector-length vec) 0))

    (define (vector= elt= . vecs)
      (or (null? vecs) (null? (cdr vecs))
          (let* ((v1 (car vecs)) (v2 (cadr vecs))
                 (len (vector-length v1)))
            (and (= len (vector-length v2))
                 (let loop ((i 0))
                   (or (>= i len)
                       (and (elt= (vector-ref v1 i) (vector-ref v2 i))
                            (loop (+ i 1)))))
                 (apply vector= elt= (cdr vecs))))))

    (define (vector-fold kons knil vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i 0) (acc knil))
              (if (>= i len) acc
                  (loop (+ i 1) (kons acc (vector-ref vec i)))))
            (let loop ((i 0) (acc knil))
              (if (>= i len) acc
                  (loop (+ i 1)
                        (apply kons acc
                          (map (lambda (v) (vector-ref v i))
                               (cons vec vecs)))))))))

    (define (vector-fold-right kons knil vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i (- len 1)) (acc knil))
              (if (< i 0) acc
                  (loop (- i 1) (kons acc (vector-ref vec i)))))
            (let loop ((i (- len 1)) (acc knil))
              (if (< i 0) acc
                  (loop (- i 1)
                        (apply kons acc
                          (map (lambda (v) (vector-ref v i))
                               (cons vec vecs)))))))))

    (define (vector-count pred vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i 0) (count 0))
              (if (>= i len) count
                  (loop (+ i 1)
                        (if (pred (vector-ref vec i)) (+ count 1) count))))
            (let loop ((i 0) (count 0))
              (if (>= i len) count
                  (loop (+ i 1)
                        (if (apply pred
                              (map (lambda (v) (vector-ref v i))
                                   (cons vec vecs)))
                            (+ count 1) count)))))))

    (define (vector-map! f vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i 0))
              (when (< i len)
                (vector-set! vec i (f (vector-ref vec i)))
                (loop (+ i 1))))
            (let loop ((i 0))
              (when (< i len)
                (vector-set! vec i
                  (apply f (map (lambda (v) (vector-ref v i))
                                (cons vec vecs))))
                (loop (+ i 1)))))))

    (define (vector-index pred vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i 0))
              (cond ((>= i len) #f)
                    ((pred (vector-ref vec i)) i)
                    (else (loop (+ i 1)))))
            (let loop ((i 0))
              (cond ((>= i len) #f)
                    ((apply pred
                       (map (lambda (v) (vector-ref v i))
                            (cons vec vecs))) i)
                    (else (loop (+ i 1))))))))

    (define (vector-index-right pred vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i (- len 1)))
              (cond ((< i 0) #f)
                    ((pred (vector-ref vec i)) i)
                    (else (loop (- i 1)))))
            (let loop ((i (- len 1)))
              (cond ((< i 0) #f)
                    ((apply pred
                       (map (lambda (v) (vector-ref v i))
                            (cons vec vecs))) i)
                    (else (loop (- i 1))))))))

    (define (vector-skip pred vec . vecs)
      (apply vector-index (lambda args (not (apply pred args)))
             vec vecs))

    (define (vector-skip-right pred vec . vecs)
      (apply vector-index-right (lambda args (not (apply pred args)))
             vec vecs))

    (define (vector-any pred vec . vecs)
      (let ((len (vector-length vec)))
        (if (null? vecs)
            (let loop ((i 0))
              (if (>= i len) #f
                  (let ((v (pred (vector-ref vec i))))
                    (if v v (loop (+ i 1))))))
            (let loop ((i 0))
              (if (>= i len) #f
                  (let ((v (apply pred
                             (map (lambda (v) (vector-ref v i))
                                  (cons vec vecs)))))
                    (if v v (loop (+ i 1)))))))))

    (define (vector-every pred vec . vecs)
      (let ((len (vector-length vec)))
        (if (= len 0) #t
            (if (null? vecs)
                (let loop ((i 0))
                  (if (= i (- len 1))
                      (pred (vector-ref vec i))
                      (if (pred (vector-ref vec i))
                          (loop (+ i 1))
                          #f)))
                (let loop ((i 0))
                  (if (= i (- len 1))
                      (apply pred
                        (map (lambda (v) (vector-ref v i))
                             (cons vec vecs)))
                      (if (apply pred
                            (map (lambda (v) (vector-ref v i))
                                 (cons vec vecs)))
                          (loop (+ i 1))
                          #f)))))))

    (define (vector-swap! vec i j)
      (let ((tmp (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j tmp)))

    (define (vector-reverse! vec . maybe-start+end)
      (let* ((start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (end (if (or (null? maybe-start+end) (null? (cdr maybe-start+end)))
                      (vector-length vec) (cadr maybe-start+end))))
        (let loop ((lo start) (hi (- end 1)))
          (when (< lo hi)
            (vector-swap! vec lo hi)
            (loop (+ lo 1) (- hi 1))))))

    (define (vector-reverse-copy vec . maybe-start+end)
      (let* ((start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (end (if (or (null? maybe-start+end) (null? (cdr maybe-start+end)))
                      (vector-length vec) (cadr maybe-start+end)))
             (len (- end start))
             (result (make-vector len)))
        (let loop ((i 0) (j (- end 1)))
          (when (< i len)
            (vector-set! result i (vector-ref vec j))
            (loop (+ i 1) (- j 1))))
        result))

    (define (vector-reverse-copy! target tstart source . maybe-start+end)
      (let* ((sstart (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (send (if (or (null? maybe-start+end) (null? (cdr maybe-start+end)))
                       (vector-length source) (cadr maybe-start+end))))
        (let loop ((i tstart) (j (- send 1)))
          (when (>= j sstart)
            (vector-set! target i (vector-ref source j))
            (loop (+ i 1) (- j 1))))))

    (define (vector-concatenate vecs)
      (apply vector-append vecs))

    (define (vector-partition pred vec)
      (let* ((len (vector-length vec))
             (yes '()) (no '()))
        (let loop ((i 0))
          (when (< i len)
            (if (pred (vector-ref vec i))
                (set! yes (cons (vector-ref vec i) yes))
                (set! no (cons (vector-ref vec i) no)))
            (loop (+ i 1))))
        (values (list->vector (reverse yes))
                (list->vector (reverse no)))))

    (define (vector-binary-search vec value cmp)
      (let loop ((lo 0) (hi (- (vector-length vec) 1)))
        (if (> lo hi) #f
            (let* ((mid (quotient (+ lo hi) 2))
                   (c (cmp (vector-ref vec mid) value)))
              (cond
                ((= c 0) mid)
                ((< c 0) (loop (+ mid 1) hi))
                (else (loop lo (- mid 1))))))))))
