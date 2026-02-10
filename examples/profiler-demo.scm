;; Profiler demo — run with:
;;   wile profile examples/profiler-demo.scm
;;   wile profile --format=flamegraph examples/profiler-demo.scm > profile.svg
;;   wile profile --format=trace examples/profiler-demo.scm > profile.json

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fib-tail n)
  (let loop ((i n) (a 0) (b 1))
    (if (= i 0) a
        (loop (- i 1) b (+ a b)))))

(define (sum-list lst)
  (let loop ((xs lst) (acc 0))
    (if (null? xs) acc
        (loop (cdr xs) (+ acc (car xs))))))

(define (make-list n)
  (let loop ((i 0) (acc '()))
    (if (= i n) acc
        (loop (+ i 1) (cons i acc)))))

(define (sort lst)
  (if (or (null? lst) (null? (cdr lst))) lst
      (let* ((pivot (car lst))
             (rest  (cdr lst))
             (lo (filter (lambda (x) (< x pivot)) rest))
             (hi (filter (lambda (x) (>= x pivot)) rest)))
        (append (sort lo) (cons pivot (sort hi))))))

(define (filter pred lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
        (if (pred (car xs))
            (loop (cdr xs) (cons (car xs) acc))
            (loop (cdr xs) acc)))))

;; Run each workload
(display "fib(25) = ")
(display (fib 25))
(newline)

(display "fib-tail(10000) = ")
(display (fib-tail 10000))
(newline)

(let ((data (make-list 500)))
  (display "sum of 0..499 = ")
  (display (sum-list data))
  (newline)
  (display "sorting 500 elements... ")
  (sort data)
  (display "done")
  (newline))
