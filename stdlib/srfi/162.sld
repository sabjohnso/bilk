(define-library (srfi 162)
  (import (scheme base) (scheme char) (srfi 128))
  (export
    comparator? make-comparator
    comparator-type-test-predicate comparator-equality-predicate
    comparator-ordering-predicate comparator-hash-function
    make-default-comparator
    =? <? >? <=? >=?
    boolean-hash char-hash string-hash number-hash symbol-hash default-hash
    boolean-comparator char-comparator char-ci-comparator
    string-comparator string-ci-comparator
    real-comparator
    default-comparator
    eq-comparator eqv-comparator equal-comparator
    comparator-max comparator-min
    make-pair-comparator make-list-comparator make-vector-comparator)
  (begin
    (define boolean-comparator
      (make-comparator boolean? boolean=?
        (lambda (a b) (and (not a) b))
        boolean-hash))
    (define char-comparator
      (make-comparator char? char=? char<? char-hash))
    (define char-ci-comparator
      (make-comparator char?
        char-ci=? char-ci<?
        (lambda (c) (char-hash (char-downcase c)))))
    (define string-comparator
      (make-comparator string? string=? string<? string-hash))
    (define string-ci-comparator
      (make-comparator string?
        string-ci=? string-ci<?
        (lambda (s) (string-hash (string-downcase s)))))
    (define real-comparator
      (make-comparator number? = < number-hash))
    (define default-comparator (make-default-comparator))
    (define eq-comparator
      (make-comparator (lambda (x) #t) eq? #f #f))
    (define eqv-comparator
      (make-comparator (lambda (x) #t) eqv? #f #f))
    (define equal-comparator
      (make-comparator (lambda (x) #t) equal? #f default-hash))

    (define (comparator-max cmp . args)
      (let loop ((result (car args)) (rest (cdr args)))
        (if (null? rest) result
            (loop (if (>? cmp (car rest) result) (car rest) result)
                  (cdr rest)))))
    (define (comparator-min cmp . args)
      (let loop ((result (car args)) (rest (cdr args)))
        (if (null? rest) result
            (loop (if (<? cmp (car rest) result) (car rest) result)
                  (cdr rest)))))

    (define (make-pair-comparator car-cmp cdr-cmp)
      (make-comparator pair?
        (lambda (a b) (and (=? car-cmp (car a) (car b))
                           (=? cdr-cmp (cdr a) (cdr b))))
        (lambda (a b) (if (=? car-cmp (car a) (car b))
                          (<? cdr-cmp (cdr a) (cdr b))
                          (<? car-cmp (car a) (car b))))
        (lambda (x) (+ (* 31 ((comparator-hash-function car-cmp) (car x)))
                       ((comparator-hash-function cdr-cmp) (cdr x))))))

    (define (make-list-comparator elt-cmp . args)
      (make-comparator list?
        (lambda (a b)
          (let loop ((a a) (b b))
            (cond ((and (null? a) (null? b)) #t)
                  ((or (null? a) (null? b)) #f)
                  ((=? elt-cmp (car a) (car b)) (loop (cdr a) (cdr b)))
                  (else #f))))
        (lambda (a b)
          (let loop ((a a) (b b))
            (cond ((null? a) (not (null? b)))
                  ((null? b) #f)
                  ((=? elt-cmp (car a) (car b)) (loop (cdr a) (cdr b)))
                  (else (<? elt-cmp (car a) (car b))))))
        (lambda (lst)
          (let loop ((lst lst) (h 0))
            (if (null? lst) h
                (loop (cdr lst)
                      (+ (* 31 h) ((comparator-hash-function elt-cmp) (car lst)))))))))

    (define (make-vector-comparator elt-cmp . args)
      (make-comparator vector?
        (lambda (a b)
          (and (= (vector-length a) (vector-length b))
               (let loop ((i 0))
                 (or (>= i (vector-length a))
                     (and (=? elt-cmp (vector-ref a i) (vector-ref b i))
                          (loop (+ i 1)))))))
        (lambda (a b)
          (let loop ((i 0))
            (cond ((>= i (min (vector-length a) (vector-length b)))
                   (< (vector-length a) (vector-length b)))
                  ((=? elt-cmp (vector-ref a i) (vector-ref b i))
                   (loop (+ i 1)))
                  (else (<? elt-cmp (vector-ref a i) (vector-ref b i))))))
        (lambda (vec)
          (let loop ((i 0) (h 0))
            (if (>= i (vector-length vec)) h
                (loop (+ i 1)
                      (+ (* 31 h)
                         ((comparator-hash-function elt-cmp) (vector-ref vec i)))))))))))
