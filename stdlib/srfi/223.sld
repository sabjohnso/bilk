(define-library (srfi 223)
  (import (scheme base) (scheme case-lambda))
  (export bisect-left bisect-right bisection
          vector-bisect-left vector-bisect-right)
  (begin
    (define (bisect-left a val ref less? lo hi)
      (let loop ((lo lo) (hi hi))
        (if (>= lo hi) lo
            (let ((mid (quotient (+ lo hi) 2)))
              (if (less? (ref a mid) val)
                  (loop (+ mid 1) hi)
                  (loop lo mid))))))

    (define (bisect-right a val ref less? lo hi)
      (let loop ((lo lo) (hi hi))
        (if (>= lo hi) lo
            (let ((mid (quotient (+ lo hi) 2)))
              (if (less? val (ref a mid))
                  (loop lo mid)
                  (loop (+ mid 1) hi))))))

    (define bisection
      (case-lambda
        ((ref)
         (bisection ref (lambda (a) (values 0 (vector-length a)))))
        ((ref lo-hi)
         (values
           (lambda (a val less?)
             (call-with-values (lambda () (lo-hi a))
               (lambda (lo hi) (bisect-left a val ref less? lo hi))))
           (lambda (a val less?)
             (call-with-values (lambda () (lo-hi a))
               (lambda (lo hi) (bisect-right a val ref less? lo hi))))))))

    (define vector-bisect-left
      (case-lambda
        ((a val less?)
         (bisect-left a val vector-ref less? 0 (vector-length a)))
        ((a val less? lo)
         (bisect-left a val vector-ref less? lo (vector-length a)))
        ((a val less? lo hi)
         (bisect-left a val vector-ref less? lo hi))))

    (define vector-bisect-right
      (case-lambda
        ((a val less?)
         (bisect-right a val vector-ref less? 0 (vector-length a)))
        ((a val less? lo)
         (bisect-right a val vector-ref less? lo (vector-length a)))
        ((a val less? lo hi)
         (bisect-right a val vector-ref less? lo hi))))))
