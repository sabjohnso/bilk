;; aot_hello.scm — A small program for ahead-of-time compilation.
;;
;; Compile and run:
;;   bilk compile examples/aot_hello.scm -o hello.fasl
;;   bilk run hello.fasl
;;
;; Or compile to a native executable (requires: opam install .):
;;   bilk compile --exe examples/aot_hello.scm -o hello
;;   ./hello

(define (fizzbuzz n)
  (let loop ((i 1))
    (when (<= i n)
      (cond
        ((= 0 (modulo i 15)) (display "FizzBuzz"))
        ((= 0 (modulo i 3))  (display "Fizz"))
        ((= 0 (modulo i 5))  (display "Buzz"))
        (else                 (display i)))
      (newline)
      (loop (+ i 1)))))

(display "FizzBuzz 1-20:")
(newline)
(fizzbuzz 20)
