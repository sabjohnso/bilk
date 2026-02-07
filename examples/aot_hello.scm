;; aot_hello.scm — A small program for ahead-of-time compilation.
;;
;; Compile and run:
;;   wile compile examples/aot_hello.scm -o hello.fasl
;;   wile run hello.fasl
;;
;; Or compile to a native executable (requires: opam install .):
;;   wile compile --exe examples/aot_hello.scm -o hello
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
