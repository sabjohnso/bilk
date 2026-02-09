(define-library (srfi 228)
  (import (scheme base) (srfi 1) (srfi 128) (srfi 151))
  (export make-wrapper-comparator make-product-comparator
          make-sum-comparator comparator-one comparator-zero)
  (begin
    (define (make-wrapper-comparator type-test unwrap cmp)
      (make-comparator type-test
        (lambda (a b) (=? cmp (unwrap a) (unwrap b)))
        (lambda (a b) (<? cmp (unwrap a) (unwrap b)))
        (lambda (x) ((comparator-hash-function cmp) (unwrap x)))))

    (define (make-product-comparator . cmps)
      (make-comparator
        (lambda (x) (every (lambda (c)
          ((comparator-type-test-predicate c) x)) cmps))
        (lambda (a b) (every (lambda (c) (=? c a b)) cmps))
        (lambda (a b)
          (let loop ((cs cmps))
            (cond ((null? cs) #f)
                  ((=? (car cs) a b) (loop (cdr cs)))
                  (else (<? (car cs) a b)))))
        (lambda (x)
          (fold (lambda (c h)
                  (bitwise-xor (arithmetic-shift h 5) h
                    ((comparator-hash-function c) x)))
                0 cmps))))

    (define (make-sum-comparator . cmps)
      ;; Returns (comparator . index) pair instead of multiple values
      ;; to avoid call-with-values issues in library context
      (define (find-cmp x)
        (let loop ((cs cmps) (i 0))
          (if (null? cs) (error "make-sum-comparator: no match" x)
              (if ((comparator-type-test-predicate (car cs)) x)
                  (cons (car cs) i)
                  (loop (cdr cs) (+ i 1))))))
      (make-comparator
        (lambda (x) (any (lambda (c)
          ((comparator-type-test-predicate c) x)) cmps))
        (lambda (a b)
          (let ((pa (find-cmp a)) (pb (find-cmp b)))
            (and (= (cdr pa) (cdr pb)) (=? (car pa) a b))))
        (lambda (a b)
          (let ((pa (find-cmp a)) (pb (find-cmp b)))
            (if (= (cdr pa) (cdr pb)) (<? (car pa) a b) (< (cdr pa) (cdr pb)))))
        (lambda (x)
          (let ((p (find-cmp x)))
            ((comparator-hash-function (car p)) x)))))

    (define comparator-one
      (make-comparator (lambda (x) #t)
        (lambda (a b) #t) (lambda (a b) #f) (lambda (x) 0)))

    (define comparator-zero
      (make-comparator (lambda (x) #f)
        (lambda (a b) (error "comparator-zero: equality"))
        (lambda (a b) (error "comparator-zero: ordering"))
        (lambda (x) (error "comparator-zero: hash"))))))
