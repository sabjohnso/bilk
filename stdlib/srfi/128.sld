(define-library (srfi 128)
  (import (scheme base) (scheme char))
  (export
    comparator? make-comparator
    comparator-type-test-predicate comparator-equality-predicate
    comparator-ordering-predicate comparator-hash-function
    make-default-comparator
    =? <? >? <=? >=?
    boolean-hash char-hash string-hash number-hash symbol-hash
    default-hash)
  (begin
    (define-record-type <comparator>
      (%make-comparator type-test equality ordering hash)
      comparator?
      (type-test comparator-type-test-predicate)
      (equality comparator-equality-predicate)
      (ordering comparator-ordering-predicate)
      (hash comparator-hash-function))

    (define (make-comparator type-test equality ordering hash)
      (%make-comparator type-test equality
        (if ordering ordering (lambda (a b) (error "comparator: no ordering")))
        (if hash hash (lambda (x) (error "comparator: no hash")))))

    (define (boolean-hash b) (if b 1 0))
    (define (char-hash c) (char->integer c))
    (define (string-hash s)
      (let ((len (string-length s)))
        (let loop ((i 0) (h 0))
          (if (>= i len) (modulo (abs h) 536870912)
              (loop (+ i 1)
                    (+ (* h 31) (char->integer (string-ref s i))))))))
    (define (number-hash n)
      (if (exact? n) (modulo (abs n) 536870912)
          (modulo (abs (exact (truncate (* n 1000000)))) 536870912)))
    (define (symbol-hash s) (string-hash (symbol->string s)))

    (define (default-hash obj)
      (cond
        ((boolean? obj) (boolean-hash obj))
        ((char? obj) (char-hash obj))
        ((string? obj) (string-hash obj))
        ((number? obj) (number-hash obj))
        ((symbol? obj) (symbol-hash obj))
        ((null? obj) 0)
        ((pair? obj) (+ (default-hash (car obj))
                        (* 31 (default-hash (cdr obj)))))
        ((vector? obj)
         (let ((len (vector-length obj)))
           (let loop ((i 0) (h 0))
             (if (>= i len) h
                 (loop (+ i 1)
                       (+ (* h 31) (default-hash (vector-ref obj i))))))))
        (else 0)))

    (define (default-type-priority obj)
      (cond
        ((null? obj) 0) ((boolean? obj) 1) ((number? obj) 2)
        ((char? obj) 3) ((string? obj) 4) ((symbol? obj) 5)
        ((pair? obj) 6) ((vector? obj) 7) ((bytevector? obj) 8)
        (else 9)))

    (define (default-ordering a b)
      (let ((pa (default-type-priority a))
            (pb (default-type-priority b)))
        (cond
          ((< pa pb) #t)
          ((> pa pb) #f)
          ((number? a) (< a b))
          ((char? a) (char<? a b))
          ((string? a) (string<? a b))
          ((symbol? a) (string<? (symbol->string a) (symbol->string b)))
          ((boolean? a) (and (not a) b))
          ((null? a) #f)
          ((pair? a)
           (or (default-ordering (car a) (car b))
               (and (equal? (car a) (car b))
                    (default-ordering (cdr a) (cdr b)))))
          (else #f))))

    (define (make-default-comparator)
      (make-comparator
        (lambda (x) #t)
        equal?
        default-ordering
        default-hash))

    (define (=? cmp a b) ((comparator-equality-predicate cmp) a b))
    (define (<? cmp a b) ((comparator-ordering-predicate cmp) a b))
    (define (>? cmp a b) (<? cmp b a))
    (define (<=? cmp a b) (or (=? cmp a b) (<? cmp a b)))
    (define (>=? cmp a b) (or (=? cmp a b) (>? cmp a b)))))
