(define-library (srfi 175)
  (import (scheme base))
  (export
    ascii-codepoint? ascii-char? ascii-string? ascii-bytevector?
    ascii-control? ascii-non-control? ascii-whitespace?
    ascii-space-or-tab? ascii-other-graphic?
    ascii-upper-case? ascii-lower-case? ascii-alphabetic?
    ascii-alphanumeric? ascii-numeric?
    ascii-digit-value ascii-upper-case-value ascii-lower-case-value
    ascii-nth-digit ascii-nth-upper-case ascii-nth-lower-case
    ascii-upcase ascii-downcase
    ascii-control->graphic ascii-graphic->control
    ascii-mirror-bracket
    ascii-ci=? ascii-ci<? ascii-ci>? ascii-ci<=? ascii-ci>=?
    ascii-string-ci=? ascii-string-ci<? ascii-string-ci>?
    ascii-string-ci<=? ascii-string-ci>=?)
  (begin
    (define (ensure-int x)
      (if (char? x) (char->integer x) x))

    (define (ascii-codepoint? x) (and (integer? x) (exact? x) (<= 0 x 127)))
    (define (ascii-char? x) (and (char? x) (<= (char->integer x) 127)))

    (define (ascii-string? s)
      (let ((len (string-length s)))
        (let loop ((i 0))
          (or (>= i len)
              (and (ascii-char? (string-ref s i))
                   (loop (+ i 1)))))))

    (define (ascii-bytevector? bv)
      (let ((len (bytevector-length bv)))
        (let loop ((i 0))
          (or (>= i len)
              (and (<= (bytevector-u8-ref bv i) 127)
                   (loop (+ i 1)))))))

    (define (ascii-control? x)
      (let ((n (ensure-int x)))
        (or (<= 0 n 31) (= n 127))))

    (define (ascii-non-control? x)
      (let ((n (ensure-int x)))
        (<= 32 n 126)))

    (define (ascii-whitespace? x)
      (let ((n (ensure-int x)))
        (or (= n 32) (= n 9) (= n 10) (= n 13) (= n 12) (= n 11))))

    (define (ascii-space-or-tab? x)
      (let ((n (ensure-int x)))
        (or (= n 32) (= n 9))))

    (define (ascii-other-graphic? x)
      (let ((n (ensure-int x)))
        (or (<= 33 n 47) (<= 58 n 64) (<= 91 n 96) (<= 123 n 126))))

    (define (ascii-upper-case? x)
      (let ((n (ensure-int x)))
        (<= 65 n 90)))

    (define (ascii-lower-case? x)
      (let ((n (ensure-int x)))
        (<= 97 n 122)))

    (define (ascii-alphabetic? x)
      (or (ascii-upper-case? x) (ascii-lower-case? x)))

    (define (ascii-numeric? x)
      (let ((n (ensure-int x)))
        (<= 48 n 57)))

    (define (ascii-alphanumeric? x)
      (or (ascii-alphabetic? x) (ascii-numeric? x)))

    (define (ascii-digit-value x)
      (let ((n (ensure-int x)))
        (if (<= 48 n 57) (- n 48) #f)))

    (define (ascii-upper-case-value x)
      (let ((n (ensure-int x)))
        (if (<= 65 n 90) (- n 65) #f)))

    (define (ascii-lower-case-value x)
      (let ((n (ensure-int x)))
        (if (<= 97 n 122) (- n 97) #f)))

    (define (ascii-nth-digit n)
      (if (<= 0 n 9) (integer->char (+ 48 n))
          (error "ascii-nth-digit: out of range" n)))

    (define (ascii-nth-upper-case n)
      (integer->char (+ 65 (modulo n 26))))

    (define (ascii-nth-lower-case n)
      (integer->char (+ 97 (modulo n 26))))

    (define (ascii-upcase x)
      (if (char? x)
          (if (ascii-lower-case? x)
              (integer->char (- (char->integer x) 32))
              x)
          (if (ascii-lower-case? x) (- x 32) x)))

    (define (ascii-downcase x)
      (if (char? x)
          (if (ascii-upper-case? x)
              (integer->char (+ (char->integer x) 32))
              x)
          (if (ascii-upper-case? x) (+ x 32) x)))

    (define (ascii-control->graphic x)
      (let ((n (ensure-int x)))
        (if (<= 0 n 31)
            (if (char? x) (integer->char (+ n 64)) (+ n 64))
            #f)))

    (define (ascii-graphic->control x)
      (let ((n (ensure-int x)))
        (if (<= 64 n 95)
            (if (char? x) (integer->char (- n 64)) (- n 64))
            #f)))

    (define (ascii-mirror-bracket x)
      (let ((n (ensure-int x)))
        (let ((r (cond
                   ((= n 40) 41) ((= n 41) 40)
                   ((= n 91) 93) ((= n 93) 91)
                   ((= n 123) 125) ((= n 125) 123)
                   ((= n 60) 62) ((= n 62) 60)
                   (else #f))))
          (if r (if (char? x) (integer->char r) r) #f))))

    (define (ascii-ci=? a b)
      (= (ensure-int (ascii-downcase a)) (ensure-int (ascii-downcase b))))
    (define (ascii-ci<? a b)
      (< (ensure-int (ascii-downcase a)) (ensure-int (ascii-downcase b))))
    (define (ascii-ci>? a b) (ascii-ci<? b a))
    (define (ascii-ci<=? a b) (not (ascii-ci>? a b)))
    (define (ascii-ci>=? a b) (not (ascii-ci<? a b)))

    (define (ascii-string-ci-cmp s1 s2)
      (let ((len1 (string-length s1))
            (len2 (string-length s2)))
        (let loop ((i 0))
          (cond
            ((and (>= i len1) (>= i len2)) 0)
            ((>= i len1) -1)
            ((>= i len2) 1)
            (else
             (let ((c1 (ensure-int (ascii-downcase (string-ref s1 i))))
                   (c2 (ensure-int (ascii-downcase (string-ref s2 i)))))
               (cond
                 ((< c1 c2) -1)
                 ((> c1 c2) 1)
                 (else (loop (+ i 1))))))))))

    (define (ascii-string-ci=? s1 s2) (= (ascii-string-ci-cmp s1 s2) 0))
    (define (ascii-string-ci<? s1 s2) (< (ascii-string-ci-cmp s1 s2) 0))
    (define (ascii-string-ci>? s1 s2) (> (ascii-string-ci-cmp s1 s2) 0))
    (define (ascii-string-ci<=? s1 s2) (<= (ascii-string-ci-cmp s1 s2) 0))
    (define (ascii-string-ci>=? s1 s2) (>= (ascii-string-ci-cmp s1 s2) 0))))
