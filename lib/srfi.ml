let sources : (string list * string) list = [

  (* SRFI 8 — receive *)
  (["srfi"; "8"], {|
(define-library (srfi 8)
  (import (scheme base))
  (export receive)
  (begin
    (define-syntax receive
      (syntax-rules ()
        ((receive formals expr body ...)
         (call-with-values (lambda () expr)
           (lambda formals body ...)))))))
|});

  (* SRFI 11 — let-values / let*-values (re-export from scheme base) *)
  (["srfi"; "11"], {|
(define-library (srfi 11)
  (import (scheme base))
  (export let-values let*-values))
|});

  (* SRFI 16 — case-lambda (re-export from scheme case-lambda) *)
  (["srfi"; "16"], {|
(define-library (srfi 16)
  (import (scheme case-lambda))
  (export case-lambda))
|});

  (* SRFI 26 — cut / cute *)
  (["srfi"; "26"], {|
(define-library (srfi 26)
  (import (scheme base))
  (export cut cute)
  (begin
    ;; cut: create procedure with partial arguments
    ;; Implemented as a Scheme procedure-based macro expansion
    ;; since the full SRFI 26 spec with <> and <...> is complex
    ;; for syntax-rules. We use a procedural approach instead.
    (define-syntax cut
      (syntax-rules (<> <...>)
        ;; 0 args to procedure
        ((cut proc)
         proc)
        ;; rest slot only
        ((cut proc <...>)
         (lambda args (apply proc args)))
        ;; 1 slot
        ((cut proc <> )
         (lambda (x) (proc x)))
        ;; 1 slot + rest
        ((cut proc <> <...>)
         (lambda (x . rest) (apply proc x rest)))
        ;; slot then arg
        ((cut proc <> a)
         (lambda (x) (proc x a)))
        ;; slot then arg then rest
        ((cut proc <> a <...>)
         (lambda (x . rest) (apply proc x a rest)))
        ;; arg then slot
        ((cut proc a <>)
         (lambda (x) (proc a x)))
        ;; arg then slot then rest
        ((cut proc a <> <...>)
         (lambda (x . rest) (apply proc a x rest)))
        ;; 1 arg + rest
        ((cut proc a <...>)
         (lambda args (apply proc a args)))
        ;; 2 args + rest
        ((cut proc a b <...>)
         (lambda args (apply proc a b args)))
        ;; 3 args + rest
        ((cut proc a b c <...>)
         (lambda args (apply proc a b c args)))
        ;; 1 arg, no slots
        ((cut proc a)
         (lambda () (proc a)))
        ;; 2 args, no slots
        ((cut proc a b)
         (lambda () (proc a b)))
        ;; 3 args, no slots
        ((cut proc a b c)
         (lambda () (proc a b c)))))

    ;; cute is the same as cut but evaluates non-slot args once
    (define-syntax cute
      (syntax-rules (<> <...>)
        ((cute proc)
         (let ((p proc)) p))
        ((cute proc <...>)
         (let ((p proc)) (lambda args (apply p args))))
        ((cute proc <>)
         (let ((p proc)) (lambda (x) (p x))))
        ((cute proc <> <...>)
         (let ((p proc)) (lambda (x . rest) (apply p x rest))))
        ((cute proc <> a)
         (let ((p proc) (v a)) (lambda (x) (p x v))))
        ((cute proc <> a <...>)
         (let ((p proc) (v a)) (lambda (x . rest) (apply p x v rest))))
        ((cute proc a <>)
         (let ((p proc) (v a)) (lambda (x) (p v x))))
        ((cute proc a <> <...>)
         (let ((p proc) (v a)) (lambda (x . rest) (apply p v x rest))))
        ((cute proc a <...>)
         (let ((p proc) (v a)) (lambda args (apply p v args))))
        ((cute proc a b <...>)
         (let ((p proc) (v1 a) (v2 b)) (lambda args (apply p v1 v2 args))))
        ((cute proc a b c <...>)
         (let ((p proc) (v1 a) (v2 b) (v3 c)) (lambda args (apply p v1 v2 v3 args))))
        ((cute proc a)
         (let ((p proc) (v a)) (lambda () (p v))))
        ((cute proc a b)
         (let ((p proc) (v1 a) (v2 b)) (lambda () (p v1 v2))))
        ((cute proc a b c)
         (let ((p proc) (v1 a) (v2 b) (v3 c)) (lambda () (p v1 v2 v3))))))))
|});

  (* SRFI 28 — basic format strings *)
  (["srfi"; "28"], {|
(define-library (srfi 28)
  (import (scheme base) (scheme write))
  (export format)
  (begin
    (define (format template . args)
      (let ((out (open-output-string))
            (len (string-length template)))
        (let loop ((i 0) (args args))
          (cond
            ((>= i len)
             (get-output-string out))
            ((and (char=? (string-ref template i) #\~)
                  (< (+ i 1) len))
             (let ((c (string-ref template (+ i 1))))
               (cond
                 ((char=? c #\a)
                  (if (null? args)
                      (error "format: not enough arguments")
                      (begin (display (car args) out)
                             (loop (+ i 2) (cdr args)))))
                 ((char=? c #\s)
                  (if (null? args)
                      (error "format: not enough arguments")
                      (begin (write (car args) out)
                             (loop (+ i 2) (cdr args)))))
                 ((char=? c #\%)
                  (newline out)
                  (loop (+ i 2) args))
                 ((char=? c #\~)
                  (write-char #\~ out)
                  (loop (+ i 2) args))
                 (else
                  (error "format: unknown escape" c)))))
            (else
             (write-char (string-ref template i) out)
             (loop (+ i 1) args))))))))
|});

  (* SRFI 31 — rec *)
  (["srfi"; "31"], {|
(define-library (srfi 31)
  (import (scheme base))
  (export rec)
  (begin
    (define-syntax rec
      (syntax-rules ()
        ((rec name expr)
         (letrec ((name expr)) name))))))
|});

  (* SRFI 111 — boxes *)
  (["srfi"; "111"], {|
(define-library (srfi 111)
  (import (scheme base))
  (export box box? unbox set-box!)
  (begin
    (define-record-type <box>
      (box value)
      box?
      (value unbox set-box!))))
|});

  (* SRFI 2 — and-let* *)
  (["srfi"; "2"], {|
(define-library (srfi 2)
  (import (scheme base))
  (export and-let*)
  (begin
    (define-syntax and-let*
      (syntax-rules ()
        ((and-let* () body ...)
         (begin #t body ...))
        ((and-let* ((name expr) rest ...) body ...)
         (let ((name expr))
           (if name (and-let* (rest ...) body ...) #f)))
        ((and-let* ((test) rest ...) body ...)
         (let ((t test))
           (if t (and-let* (rest ...) body ...) #f)))))))
|});

  (* SRFI 128 — comparators *)
  (["srfi"; "128"], {|
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
|});

  (* SRFI 132 — sort libraries *)
  (["srfi"; "132"], {|
(define-library (srfi 132)
  (import (scheme base))
  (export
    list-sorted? vector-sorted?
    list-sort vector-sort list-stable-sort vector-stable-sort
    list-sort! vector-sort! list-stable-sort! vector-stable-sort!
    list-merge vector-merge list-merge! vector-merge!
    list-delete-neighbor-dups vector-delete-neighbor-dups
    list-delete-neighbor-dups! vector-delete-neighbor-dups!)
  (begin
    (define (list-sorted? less? lst)
      (or (null? lst) (null? (cdr lst))
          (and (not (less? (cadr lst) (car lst)))
               (list-sorted? less? (cdr lst)))))

    (define (vector-sorted? less? vec . maybe-start+end)
      (let* ((start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (end (if (or (null? maybe-start+end) (null? (cdr maybe-start+end)))
                      (vector-length vec) (cadr maybe-start+end))))
        (or (<= (- end start) 1)
            (let loop ((i (+ start 1)))
              (or (>= i end)
                  (and (not (less? (vector-ref vec i) (vector-ref vec (- i 1))))
                       (loop (+ i 1))))))))

    (define (list-merge less? a b)
      (cond
        ((null? a) b)
        ((null? b) a)
        ((less? (car b) (car a))
         (cons (car b) (list-merge less? a (cdr b))))
        (else
         (cons (car a) (list-merge less? (cdr a) b)))))

    (define (list-sort less? lst)
      (let ((len (length lst)))
        (if (<= len 1) lst
            (let* ((mid (quotient len 2))
                   (left (list-sort less? (take-list lst mid)))
                   (right (list-sort less? (drop-list lst mid))))
              (list-merge less? left right)))))

    (define (take-list lst n)
      (if (= n 0) '()
          (cons (car lst) (take-list (cdr lst) (- n 1)))))
    (define (drop-list lst n)
      (if (= n 0) lst (drop-list (cdr lst) (- n 1))))

    (define (list-stable-sort less? lst) (list-sort less? lst))
    (define (list-sort! less? lst) (list-sort less? lst))
    (define (list-stable-sort! less? lst) (list-sort less? lst))

    (define (vector-sort less? vec . maybe-start+end)
      (let* ((start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (end (if (or (null? maybe-start+end) (null? (cdr maybe-start+end)))
                      (vector-length vec) (cadr maybe-start+end)))
             (lst (let loop ((i start) (acc '()))
                    (if (>= i end) (reverse acc)
                        (loop (+ i 1) (cons (vector-ref vec i) acc))))))
        (list->vector (list-sort less? lst))))

    (define (vector-stable-sort less? vec . maybe-start+end)
      (apply vector-sort less? vec maybe-start+end))
    (define (vector-sort! less? vec . maybe-start+end)
      (let ((sorted (apply vector-sort less? vec maybe-start+end))
            (start (if (null? maybe-start+end) 0 (car maybe-start+end))))
        (let loop ((i 0))
          (when (< i (vector-length sorted))
            (vector-set! vec (+ start i) (vector-ref sorted i))
            (loop (+ i 1))))))
    (define (vector-stable-sort! less? vec . maybe-start+end)
      (apply vector-sort! less? vec maybe-start+end))

    (define (list-merge! less? a b) (list-merge less? a b))

    (define (vector-merge less? vec1 vec2)
      (list->vector (list-merge less? (vector->list vec1) (vector->list vec2))))
    (define (vector-merge! less? target tstart vec1 vec2)
      (let ((merged (vector-merge less? vec1 vec2)))
        (let loop ((i 0))
          (when (< i (vector-length merged))
            (vector-set! target (+ tstart i) (vector-ref merged i))
            (loop (+ i 1))))))

    (define (list-delete-neighbor-dups eq lst)
      (cond
        ((null? lst) '())
        ((null? (cdr lst)) lst)
        ((eq (car lst) (cadr lst))
         (list-delete-neighbor-dups eq (cdr lst)))
        (else (cons (car lst)
                    (list-delete-neighbor-dups eq (cdr lst))))))

    (define (list-delete-neighbor-dups! eq lst)
      (list-delete-neighbor-dups eq lst))

    (define (vector-delete-neighbor-dups eq vec . maybe-start+end)
      (let* ((start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (end (if (or (null? maybe-start+end) (null? (cdr maybe-start+end)))
                      (vector-length vec) (cadr maybe-start+end))))
        (list->vector
          (list-delete-neighbor-dups eq
            (let loop ((i start) (acc '()))
              (if (>= i end) (reverse acc)
                  (loop (+ i 1) (cons (vector-ref vec i) acc))))))))

    (define (vector-delete-neighbor-dups! eq vec . maybe-start+end)
      (apply vector-delete-neighbor-dups eq vec maybe-start+end))))
|});

  (* SRFI 133 — vector library *)
  (["srfi"; "133"], {|
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
|});

  (* SRFI 125 — intermediate hash tables *)
  (["srfi"; "125"], {|
(define-library (srfi 125)
  (import (scheme base) (srfi 69) (srfi 128))
  (export
    ;; from SRFI 69 (re-export)
    hash-table? hash-table-ref hash-table-ref/default
    hash-table-set! hash-table-delete! hash-table-exists?
    hash-table-update! hash-table-update!/default
    hash-table-size hash-table-keys hash-table-values
    hash-table-walk hash-table-fold hash-table->alist
    hash-table-copy hash-table-merge!
    hash string-hash string-ci-hash hash-by-identity
    ;; from SRFI 128 (re-export hash functions)
    default-hash
    ;; SRFI 125 new procedures
    make-hash-table hash-table hash-table-unfold
    hash-table-contains? hash-table-empty?
    hash-table=? hash-table-mutable?
    hash-table-intern!
    hash-table-pop!
    hash-table-for-each hash-table-map hash-table-map!
    hash-table-map->list hash-table-prune!
    hash-table-count hash-table-find
    hash-table-entries
    hash-table-union! hash-table-intersection!
    hash-table-difference! hash-table-xor!
    hash-table-empty-copy
    alist->hash-table
    hash-table-equivalence-function hash-table-hash-function
    hash-table-clear!)
  (begin
    ;; Capture the raw SRFI 69 make-hash-table before we shadow it
    (define %raw-make-hash-table make-hash-table)

    ;; make-hash-table: accept comparator or eq/hash procs
    (define (make-hash-table . args)
      (cond
        ((null? args)
         (%raw-make-hash-table))
        ((comparator? (car args))
         (let ((cmp (car args))
               (cap (if (null? (cdr args)) 16 (cadr args))))
           (%raw-make-hash-table
            (comparator-equality-predicate cmp)
            (comparator-hash-function cmp)
            cap)))
        (else
         (apply %raw-make-hash-table args))))

    ;; hash-table constructor: (hash-table comparator k1 v1 k2 v2 ...)
    (define (hash-table comparator . kvs)
      (let ((ht (make-hash-table comparator)))
        (let loop ((rest kvs))
          (if (null? rest) ht
              (if (null? (cdr rest))
                  (error "hash-table: odd number of key/value arguments")
                  (begin
                    (hash-table-set! ht (car rest) (cadr rest))
                    (loop (cddr rest))))))))

    ;; hash-table-unfold
    (define (hash-table-unfold stop? mapper successor seed comparator)
      (let ((ht (make-hash-table comparator)))
        (let loop ((s seed))
          (if (stop? s) ht
              (let ((kv (call-with-values (lambda () (mapper s)) list)))
                (hash-table-set! ht (car kv) (cadr kv))
                (loop (successor s)))))))

    ;; predicates
    (define (hash-table-contains? ht key)
      (hash-table-exists? ht key))

    (define (hash-table-empty? ht)
      (= 0 (hash-table-size ht)))

    ;; hash-table=?
    (define (hash-table=? value-cmp ht1 ht2)
      (and (= (hash-table-size ht1) (hash-table-size ht2))
           (let ((result #t))
             (hash-table-walk ht1
               (lambda (k v1)
                 (when result
                   (if (hash-table-exists? ht2 k)
                       (unless (value-cmp v1 (hash-table-ref ht2 k))
                         (set! result #f))
                       (set! result #f)))))
             result)))

    ;; hash-table-intern!
    (define (hash-table-intern! ht key failure)
      (if (hash-table-exists? ht key)
          (hash-table-ref ht key)
          (let ((v (failure)))
            (hash-table-set! ht key v)
            v)))

    ;; hash-table-pop!
    (define (hash-table-pop! ht)
      (let ((keys (hash-table-keys ht)))
        (if (null? keys)
            (error "hash-table-pop!: hash table is empty")
            (let* ((k (car keys))
                   (v (hash-table-ref ht k)))
              (hash-table-delete! ht k)
              (values k v)))))

    ;; iteration
    (define (hash-table-for-each proc ht)
      (hash-table-walk ht proc))

    (define (hash-table-map proc comparator ht)
      (let ((result (make-hash-table comparator)))
        (hash-table-walk ht
          (lambda (k v)
            (hash-table-set! result k (proc v))))
        result))

    (define (hash-table-map! proc ht)
      (hash-table-walk ht
        (lambda (k v)
          (hash-table-set! ht k (proc v)))))

    (define (hash-table-map->list proc ht)
      (hash-table-fold ht
        (lambda (k v acc) (cons (proc k v) acc))
        '()))

    (define (hash-table-prune! pred ht)
      (let ((to-delete '()))
        (hash-table-walk ht
          (lambda (k v)
            (when (pred k v)
              (set! to-delete (cons k to-delete)))))
        (for-each (lambda (k) (hash-table-delete! ht k)) to-delete)))

    ;; counting / searching
    (define (hash-table-count pred ht)
      (hash-table-fold ht
        (lambda (k v acc) (if (pred k v) (+ acc 1) acc))
        0))

    (define (hash-table-find proc ht failure)
      (call-with-current-continuation
        (lambda (return)
          (hash-table-walk ht
            (lambda (k v)
              (when (proc k v)
                (return v))))
          (failure))))

    ;; entries
    (define (hash-table-entries ht)
      (values (hash-table-keys ht) (hash-table-values ht)))

    ;; set operations
    (define (hash-table-union! ht1 ht2)
      (hash-table-walk ht2
        (lambda (k v)
          (unless (hash-table-exists? ht1 k)
            (hash-table-set! ht1 k v))))
      ht1)

    (define (hash-table-intersection! ht1 ht2)
      (let ((to-delete '()))
        (hash-table-walk ht1
          (lambda (k v)
            (unless (hash-table-exists? ht2 k)
              (set! to-delete (cons k to-delete)))))
        (for-each (lambda (k) (hash-table-delete! ht1 k)) to-delete))
      ht1)

    (define (hash-table-difference! ht1 ht2)
      (let ((to-delete '()))
        (hash-table-walk ht1
          (lambda (k v)
            (when (hash-table-exists? ht2 k)
              (set! to-delete (cons k to-delete)))))
        (for-each (lambda (k) (hash-table-delete! ht1 k)) to-delete))
      ht1)

    (define (hash-table-xor! ht1 ht2)
      (hash-table-walk ht2
        (lambda (k v)
          (if (hash-table-exists? ht1 k)
              (hash-table-delete! ht1 k)
              (hash-table-set! ht1 k v))))
      ht1)

    ;; empty copy
    (define (hash-table-empty-copy ht)
      (%raw-make-hash-table
       (hash-table-equivalence-function ht)
       (hash-table-hash-function ht)))))
|});

  (* SRFI 41 — streams (primitive) *)
  (["srfi"; "41"; "primitive"], {|
(define-library (srfi 41 primitive)
  (import (scheme base) (scheme lazy))
  (export stream-null stream-cons stream? stream-null?
         stream-pair? stream-car stream-cdr stream-lambda
         make-stream-promise stream-type-promise)
  (begin
    (define-record-type <stream-type>
      (make-stream-promise promise)
      stream-type?
      (promise stream-type-promise))
    (define stream-null (make-stream-promise (delay '())))
    (define-syntax stream-cons
      (syntax-rules ()
        ((stream-cons a b)
         (make-stream-promise (delay (cons a b))))))
    (define (stream? x) (stream-type? x))
    (define (stream-null? x)
      (and (stream? x)
           (null? (force (stream-type-promise x)))))
    (define (stream-pair? x)
      (and (stream? x)
           (pair? (force (stream-type-promise x)))))
    (define (stream-car s)
      (if (stream-null? s)
          (error "stream-car: empty stream")
          (car (force (stream-type-promise s)))))
    (define (stream-cdr s)
      (if (stream-null? s)
          (error "stream-cdr: empty stream")
          (cdr (force (stream-type-promise s)))))
    (define-syntax stream-lambda
      (syntax-rules ()
        ((stream-lambda formals body ...)
         (lambda formals
           (make-stream-promise (delay (begin body ...)))))))))
|});

  (* SRFI 41 — streams (full) *)
  (["srfi"; "41"], {|
(define-library (srfi 41)
  (import (scheme base) (scheme lazy) (srfi 41 primitive))
  (export
    stream-null stream-cons stream? stream-null?
    stream-pair? stream-car stream-cdr stream-lambda
    make-stream-promise stream-type-promise
    stream stream-unfold stream-map stream-for-each
    stream-filter stream-fold stream-take stream-take-while
    stream-drop stream-drop-while stream-zip
    stream-constant stream-range stream-iterate
    stream-append stream-concat stream-scan
    stream-length stream->list list->stream)
  (begin
    (define-syntax stream
      (syntax-rules ()
        ((stream) stream-null)
        ((stream x rest ...)
         (stream-cons x (stream rest ...)))))

    (define (stream-unfold map? base gen seed)
      (let loop ((s seed))
        (if (map? s)
            (stream-cons (base s) (loop (gen s)))
            stream-null)))

    (define (stream-map proc s . rest)
      (if (null? rest)
          (let loop ((s s))
            (if (stream-null? s) stream-null
                (stream-cons (proc (stream-car s))
                             (loop (stream-cdr s)))))
          (let loop ((streams (cons s rest)))
            (if (any stream-null? streams) stream-null
                (stream-cons
                  (apply proc (map stream-car streams))
                  (loop (map stream-cdr streams)))))))

    (define (any pred lst)
      (cond ((null? lst) #f)
            ((pred (car lst)) #t)
            (else (any pred (cdr lst)))))

    (define (stream-for-each proc s . rest)
      (if (null? rest)
          (let loop ((s s))
            (unless (stream-null? s)
              (proc (stream-car s))
              (loop (stream-cdr s))))
          (let loop ((streams (cons s rest)))
            (unless (any stream-null? streams)
              (apply proc (map stream-car streams))
              (loop (map stream-cdr streams))))))

    (define (stream-filter pred s)
      (let loop ((s s))
        (cond
          ((stream-null? s) stream-null)
          ((pred (stream-car s))
           (stream-cons (stream-car s) (loop (stream-cdr s))))
          (else (loop (stream-cdr s))))))

    (define (stream-fold proc base s)
      (let loop ((s s) (acc base))
        (if (stream-null? s) acc
            (loop (stream-cdr s) (proc acc (stream-car s))))))

    (define (stream-take n s)
      (let loop ((n n) (s s))
        (if (or (= n 0) (stream-null? s)) stream-null
            (stream-cons (stream-car s)
                         (loop (- n 1) (stream-cdr s))))))

    (define (stream-take-while pred s)
      (let loop ((s s))
        (if (or (stream-null? s)
                (not (pred (stream-car s))))
            stream-null
            (stream-cons (stream-car s)
                         (loop (stream-cdr s))))))

    (define (stream-drop n s)
      (let loop ((n n) (s s))
        (if (or (= n 0) (stream-null? s)) s
            (loop (- n 1) (stream-cdr s)))))

    (define (stream-drop-while pred s)
      (let loop ((s s))
        (if (or (stream-null? s)
                (not (pred (stream-car s))))
            s
            (loop (stream-cdr s)))))

    (define (stream-zip s . rest)
      (let loop ((streams (cons s rest)))
        (if (any stream-null? streams) stream-null
            (stream-cons
              (map stream-car streams)
              (loop (map stream-cdr streams))))))

    (define (stream-constant . objs)
      (let ((lst objs))
        (let loop ((rest lst))
          (if (null? rest)
              (loop lst)
              (stream-cons (car rest) (loop (cdr rest)))))))

    (define (stream-range first past . maybe-step)
      (let ((step (if (null? maybe-step) 1 (car maybe-step))))
        (let loop ((n first))
          (if (if (> step 0) (>= n past) (<= n past))
              stream-null
              (stream-cons n (loop (+ n step)))))))

    (define (stream-iterate proc base)
      (stream-cons base (stream-iterate proc (proc base))))

    (define (stream-append . streams)
      (if (null? streams) stream-null
          (let loop ((s (car streams)) (rest (cdr streams)))
            (if (stream-null? s)
                (if (null? rest) stream-null
                    (loop (car rest) (cdr rest)))
                (stream-cons (stream-car s)
                             (loop (stream-cdr s) rest))))))

    (define (stream-concat s)
      (let loop ((s s))
        (if (stream-null? s) stream-null
            (let inner ((cur (stream-car s)))
              (if (stream-null? cur)
                  (loop (stream-cdr s))
                  (stream-cons (stream-car cur)
                               (inner (stream-cdr cur))))))))

    (define (stream-scan proc base s)
      (stream-cons base
        (if (stream-null? s) stream-null
            (stream-scan proc
                         (proc base (stream-car s))
                         (stream-cdr s)))))

    (define (stream-length s)
      (let loop ((s s) (n 0))
        (if (stream-null? s) n
            (loop (stream-cdr s) (+ n 1)))))

    (define (stream->list . args)
      (let ((s (if (null? (cdr args)) (car args) (cadr args)))
            (n (if (null? (cdr args)) -1 (car args))))
        (let loop ((s s) (n n) (acc '()))
          (if (or (stream-null? s) (= n 0))
              (reverse acc)
              (loop (stream-cdr s) (- n 1)
                    (cons (stream-car s) acc))))))

    (define (list->stream lst)
      (if (null? lst) stream-null
          (stream-cons (car lst) (list->stream (cdr lst)))))))
|});

  (* SRFI 113 — sets and bags *)
  (["srfi"; "113"], {|
(define-library (srfi 113)
  (import (scheme base) (srfi 69) (srfi 128))
  (export
    set set? set-contains? set-empty? set-disjoint?
    set-member set-element-comparator
    set-adjoin set-adjoin! set-replace set-replace!
    set-delete set-delete! set-delete-all set-delete-all!
    set-size set-find set-count set-any? set-every?
    set-map set-for-each set-fold
    set-filter set-filter! set-remove set-remove!
    set-copy set->list list->set
    set-union set-union! set-intersection set-intersection!
    set-difference set-difference! set-xor set-xor!
    set=? set<? set>? set<=? set>=?
    bag bag? bag-contains? bag-empty? bag-disjoint?
    bag-member bag-element-comparator
    bag-adjoin bag-adjoin! bag-replace bag-replace!
    bag-delete bag-delete! bag-delete-all bag-delete-all!
    bag-size bag-find bag-count bag-any? bag-every?
    bag-map bag-for-each bag-fold
    bag-filter bag-filter! bag-remove bag-remove!
    bag-copy bag->list list->bag
    bag-union bag-union! bag-intersection bag-intersection!
    bag-difference bag-difference! bag-xor bag-xor!
    bag=? bag<? bag>? bag<=? bag>=?
    bag-element-count bag-for-each-unique bag-fold-unique
    bag-increment! bag-decrement!
    bag->set set->bag bag->alist alist->bag)
  (begin
    ;; -- Set type --
    (define-record-type <set>
      (%make-set ht comparator)
      set?
      (ht set-ht)
      (comparator set-comparator))

    (define (make-set-ht cmp)
      (make-hash-table (comparator-equality-predicate cmp)
                       (comparator-hash-function cmp)))

    (define (set comparator . elts)
      (let ((s (%make-set (make-set-ht comparator) comparator)))
        (for-each (lambda (e) (hash-table-set! (set-ht s) e #t)) elts)
        s))

    (define (set-contains? s e) (hash-table-exists? (set-ht s) e))
    (define (set-empty? s) (= 0 (hash-table-size (set-ht s))))
    (define (set-size s) (hash-table-size (set-ht s)))

    (define (set-member s e default)
      (if (hash-table-exists? (set-ht s) e) e default))

    (define (set-element-comparator s) (set-comparator s))

    (define (set-disjoint? s1 s2)
      (let ((result #t))
        (hash-table-walk (set-ht s1)
          (lambda (k v)
            (when (hash-table-exists? (set-ht s2) k)
              (set! result #f))))
        result))

    (define (set-adjoin s . elts)
      (let ((r (set-copy s)))
        (for-each (lambda (e) (hash-table-set! (set-ht r) e #t)) elts)
        r))
    (define (set-adjoin! s . elts)
      (for-each (lambda (e) (hash-table-set! (set-ht s) e #t)) elts)
      s)

    (define (set-replace s e)
      (let ((r (set-copy s)))
        (hash-table-set! (set-ht r) e #t) r))
    (define (set-replace! s e)
      (hash-table-set! (set-ht s) e #t) s)

    (define (set-delete s . elts)
      (let ((r (set-copy s)))
        (for-each (lambda (e) (hash-table-delete! (set-ht r) e)) elts)
        r))
    (define (set-delete! s . elts)
      (for-each (lambda (e) (hash-table-delete! (set-ht s) e)) elts)
      s)
    (define (set-delete-all s elt-list)
      (apply set-delete s elt-list))
    (define (set-delete-all! s elt-list)
      (apply set-delete! s elt-list))

    (define (set-find pred s failure)
      (let ((keys (hash-table-keys (set-ht s))))
        (let loop ((keys keys))
          (cond
            ((null? keys) (failure))
            ((pred (car keys)) (car keys))
            (else (loop (cdr keys)))))))

    (define (set-count pred s)
      (hash-table-fold (set-ht s)
        (lambda (k v acc) (if (pred k) (+ acc 1) acc)) 0))

    (define (set-any? pred s)
      (let ((keys (hash-table-keys (set-ht s))))
        (let loop ((keys keys))
          (cond
            ((null? keys) #f)
            ((pred (car keys)) #t)
            (else (loop (cdr keys)))))))

    (define (set-every? pred s)
      (let ((keys (hash-table-keys (set-ht s))))
        (let loop ((keys keys))
          (cond
            ((null? keys) #t)
            ((not (pred (car keys))) #f)
            (else (loop (cdr keys)))))))

    (define (set-map cmp proc s)
      (let ((r (%make-set (make-set-ht cmp) cmp)))
        (hash-table-walk (set-ht s)
          (lambda (k v) (hash-table-set! (set-ht r) (proc k) #t)))
        r))

    (define (set-for-each proc s)
      (hash-table-walk (set-ht s) (lambda (k v) (proc k))))

    (define (set-fold proc nil s)
      (hash-table-fold (set-ht s) (lambda (k v acc) (proc k acc)) nil))

    (define (set-filter pred s)
      (let ((r (%make-set (make-set-ht (set-comparator s)) (set-comparator s))))
        (hash-table-walk (set-ht s)
          (lambda (k v) (when (pred k) (hash-table-set! (set-ht r) k #t))))
        r))
    (define (set-filter! pred s)
      (let ((to-del '()))
        (hash-table-walk (set-ht s)
          (lambda (k v) (unless (pred k) (set! to-del (cons k to-del)))))
        (for-each (lambda (k) (hash-table-delete! (set-ht s) k)) to-del)
        s))
    (define (set-remove pred s)
      (set-filter (lambda (x) (not (pred x))) s))
    (define (set-remove! pred s)
      (set-filter! (lambda (x) (not (pred x))) s))

    (define (set-copy s)
      (let ((r (%make-set (make-set-ht (set-comparator s)) (set-comparator s))))
        (hash-table-walk (set-ht s)
          (lambda (k v) (hash-table-set! (set-ht r) k v)))
        r))

    (define (set->list s) (hash-table-keys (set-ht s)))
    (define (list->set cmp lst)
      (apply set cmp lst))

    (define (set-union s . rest)
      (let ((r (set-copy s)))
        (for-each (lambda (s2)
          (hash-table-walk (set-ht s2)
            (lambda (k v) (hash-table-set! (set-ht r) k #t))))
          rest)
        r))
    (define (set-union! s . rest)
      (for-each (lambda (s2)
        (hash-table-walk (set-ht s2)
          (lambda (k v) (hash-table-set! (set-ht s) k #t))))
        rest)
      s)
    (define (set-intersection s . rest)
      (let ((r (set-copy s)))
        (let ((to-del '()))
          (hash-table-walk (set-ht r)
            (lambda (k v)
              (unless (every (lambda (s2) (hash-table-exists? (set-ht s2) k)) rest)
                (set! to-del (cons k to-del)))))
          (for-each (lambda (k) (hash-table-delete! (set-ht r) k)) to-del))
        r))
    (define (set-intersection! s . rest)
      (let ((to-del '()))
        (hash-table-walk (set-ht s)
          (lambda (k v)
            (unless (every (lambda (s2) (hash-table-exists? (set-ht s2) k)) rest)
              (set! to-del (cons k to-del)))))
        (for-each (lambda (k) (hash-table-delete! (set-ht s) k)) to-del))
      s)
    (define (set-difference s . rest)
      (let ((r (set-copy s)))
        (for-each (lambda (s2)
          (hash-table-walk (set-ht s2)
            (lambda (k v) (hash-table-delete! (set-ht r) k))))
          rest)
        r))
    (define (set-difference! s . rest)
      (for-each (lambda (s2)
        (hash-table-walk (set-ht s2)
          (lambda (k v) (hash-table-delete! (set-ht s) k))))
        rest)
      s)
    (define (set-xor s1 s2)
      (let ((r (%make-set (make-set-ht (set-comparator s1)) (set-comparator s1))))
        (hash-table-walk (set-ht s1)
          (lambda (k v)
            (unless (hash-table-exists? (set-ht s2) k)
              (hash-table-set! (set-ht r) k #t))))
        (hash-table-walk (set-ht s2)
          (lambda (k v)
            (unless (hash-table-exists? (set-ht s1) k)
              (hash-table-set! (set-ht r) k #t))))
        r))
    (define (set-xor! s1 s2)
      (let ((to-del '()) (to-add '()))
        (hash-table-walk (set-ht s1)
          (lambda (k v)
            (when (hash-table-exists? (set-ht s2) k)
              (set! to-del (cons k to-del)))))
        (hash-table-walk (set-ht s2)
          (lambda (k v)
            (unless (hash-table-exists? (set-ht s1) k)
              (set! to-add (cons k to-add)))))
        (for-each (lambda (k) (hash-table-delete! (set-ht s1) k)) to-del)
        (for-each (lambda (k) (hash-table-set! (set-ht s1) k #t)) to-add))
      s1)

    ;; set comparisons
    (define (every pred lst)
      (cond ((null? lst) #t)
            ((not (pred (car lst))) #f)
            (else (every pred (cdr lst)))))

    (define (set=? s1 s2)
      (and (= (set-size s1) (set-size s2))
           (every (lambda (k) (set-contains? s2 k)) (set->list s1))))
    (define (set<? s1 s2)
      (and (< (set-size s1) (set-size s2))
           (every (lambda (k) (set-contains? s2 k)) (set->list s1))))
    (define (set>? s1 s2) (set<? s2 s1))
    (define (set<=? s1 s2)
      (every (lambda (k) (set-contains? s2 k)) (set->list s1)))
    (define (set>=? s1 s2) (set<=? s2 s1))

    ;; -- Bag type --
    (define-record-type <bag>
      (%make-bag ht comparator)
      bag?
      (ht bag-ht)
      (comparator bag-comparator))

    (define (make-bag-ht cmp)
      (make-hash-table (comparator-equality-predicate cmp)
                       (comparator-hash-function cmp)))

    (define (bag comparator . elts)
      (let ((b (%make-bag (make-bag-ht comparator) comparator)))
        (for-each (lambda (e) (bag-adjoin! b e)) elts)
        b))

    (define (bag-contains? b e) (hash-table-exists? (bag-ht b) e))
    (define (bag-empty? b) (= 0 (hash-table-size (bag-ht b))))

    (define (bag-size b)
      (hash-table-fold (bag-ht b) (lambda (k v acc) (+ acc v)) 0))

    (define (bag-element-count b e)
      (if (hash-table-exists? (bag-ht b) e)
          (hash-table-ref (bag-ht b) e) 0))

    (define (bag-member b e default)
      (if (hash-table-exists? (bag-ht b) e) e default))

    (define (bag-element-comparator b) (bag-comparator b))

    (define (bag-disjoint? b1 b2)
      (let ((result #t))
        (hash-table-walk (bag-ht b1)
          (lambda (k v)
            (when (hash-table-exists? (bag-ht b2) k)
              (set! result #f))))
        result))

    (define (bag-adjoin b . elts)
      (let ((r (bag-copy b)))
        (for-each (lambda (e) (bag-adjoin! r e)) elts)
        r))
    (define (bag-adjoin! b . elts)
      (for-each (lambda (e)
        (let ((n (bag-element-count b e)))
          (hash-table-set! (bag-ht b) e (+ n 1))))
        elts)
      b)

    (define (bag-replace b e) (bag-adjoin b e))
    (define (bag-replace! b e) (bag-adjoin! b e))

    (define (bag-delete b . elts)
      (let ((r (bag-copy b)))
        (for-each (lambda (e) (bag-delete! r e)) elts)
        r))
    (define (bag-delete! b . elts)
      (for-each (lambda (e)
        (let ((n (bag-element-count b e)))
          (if (<= n 1)
              (hash-table-delete! (bag-ht b) e)
              (hash-table-set! (bag-ht b) e (- n 1)))))
        elts)
      b)
    (define (bag-delete-all b elt-list)
      (apply bag-delete b elt-list))
    (define (bag-delete-all! b elt-list)
      (apply bag-delete! b elt-list))

    (define (bag-find pred b failure)
      (let ((keys (hash-table-keys (bag-ht b))))
        (let loop ((keys keys))
          (cond
            ((null? keys) (failure))
            ((pred (car keys)) (car keys))
            (else (loop (cdr keys)))))))

    (define (bag-count pred b)
      (hash-table-fold (bag-ht b)
        (lambda (k v acc) (if (pred k) (+ acc v) acc)) 0))

    (define (bag-any? pred b)
      (let ((keys (hash-table-keys (bag-ht b))))
        (let loop ((keys keys))
          (cond
            ((null? keys) #f)
            ((pred (car keys)) #t)
            (else (loop (cdr keys)))))))

    (define (bag-every? pred b)
      (let ((keys (hash-table-keys (bag-ht b))))
        (let loop ((keys keys))
          (cond
            ((null? keys) #t)
            ((not (pred (car keys))) #f)
            (else (loop (cdr keys)))))))

    (define (bag-map cmp proc b)
      (let ((r (%make-bag (make-bag-ht cmp) cmp)))
        (hash-table-walk (bag-ht b)
          (lambda (k v)
            (let ((nk (proc k)))
              (hash-table-set! (bag-ht r) nk
                (+ (bag-element-count r nk) v)))))
        r))

    (define (bag-for-each proc b)
      (hash-table-walk (bag-ht b) (lambda (k v) (proc k))))

    (define (bag-fold proc nil b)
      (hash-table-fold (bag-ht b)
        (lambda (k v acc)
          (let loop ((n v) (a acc))
            (if (= n 0) a
                (loop (- n 1) (proc k a)))))
        nil))

    (define (bag-filter pred b)
      (let ((r (%make-bag (make-bag-ht (bag-comparator b)) (bag-comparator b))))
        (hash-table-walk (bag-ht b)
          (lambda (k v) (when (pred k) (hash-table-set! (bag-ht r) k v))))
        r))
    (define (bag-filter! pred b)
      (let ((to-del '()))
        (hash-table-walk (bag-ht b)
          (lambda (k v) (unless (pred k) (set! to-del (cons k to-del)))))
        (for-each (lambda (k) (hash-table-delete! (bag-ht b) k)) to-del)
        b))
    (define (bag-remove pred b)
      (bag-filter (lambda (x) (not (pred x))) b))
    (define (bag-remove! pred b)
      (bag-filter! (lambda (x) (not (pred x))) b))

    (define (bag-copy b)
      (let ((r (%make-bag (make-bag-ht (bag-comparator b)) (bag-comparator b))))
        (hash-table-walk (bag-ht b)
          (lambda (k v) (hash-table-set! (bag-ht r) k v)))
        r))

    (define (bag->list b)
      (hash-table-fold (bag-ht b)
        (lambda (k v acc)
          (let loop ((n v) (a acc))
            (if (= n 0) a
                (loop (- n 1) (cons k a)))))
        '()))
    (define (list->bag cmp lst)
      (apply bag cmp lst))

    (define (bag-union b . rest)
      (let ((r (bag-copy b)))
        (for-each (lambda (b2)
          (hash-table-walk (bag-ht b2)
            (lambda (k v)
              (let ((cur (bag-element-count r k)))
                (hash-table-set! (bag-ht r) k (max cur v))))))
          rest)
        r))
    (define (bag-union! b . rest)
      (for-each (lambda (b2)
        (hash-table-walk (bag-ht b2)
          (lambda (k v)
            (let ((cur (bag-element-count b k)))
              (hash-table-set! (bag-ht b) k (max cur v))))))
        rest)
      b)
    (define (bag-intersection b . rest)
      (let ((r (bag-copy b)))
        (let ((to-del '()))
          (hash-table-walk (bag-ht r)
            (lambda (k v)
              (let ((mn (apply min v
                          (map (lambda (b2) (bag-element-count b2 k)) rest))))
                (if (= mn 0)
                    (set! to-del (cons k to-del))
                    (hash-table-set! (bag-ht r) k mn)))))
          (for-each (lambda (k) (hash-table-delete! (bag-ht r) k)) to-del))
        r))
    (define (bag-intersection! b . rest)
      (let ((to-del '()))
        (hash-table-walk (bag-ht b)
          (lambda (k v)
            (let ((mn (apply min v
                        (map (lambda (b2) (bag-element-count b2 k)) rest))))
              (if (= mn 0)
                  (set! to-del (cons k to-del))
                  (hash-table-set! (bag-ht b) k mn)))))
        (for-each (lambda (k) (hash-table-delete! (bag-ht b) k)) to-del))
      b)
    (define (bag-difference b . rest)
      (let ((r (bag-copy b)))
        (for-each (lambda (b2)
          (hash-table-walk (bag-ht b2)
            (lambda (k v) (hash-table-delete! (bag-ht r) k))))
          rest)
        r))
    (define (bag-difference! b . rest)
      (for-each (lambda (b2)
        (hash-table-walk (bag-ht b2)
          (lambda (k v) (hash-table-delete! (bag-ht b) k))))
        rest)
      b)
    (define (bag-xor b1 b2)
      (let ((r (%make-bag (make-bag-ht (bag-comparator b1)) (bag-comparator b1))))
        (hash-table-walk (bag-ht b1)
          (lambda (k v)
            (unless (hash-table-exists? (bag-ht b2) k)
              (hash-table-set! (bag-ht r) k v))))
        (hash-table-walk (bag-ht b2)
          (lambda (k v)
            (unless (hash-table-exists? (bag-ht b1) k)
              (hash-table-set! (bag-ht r) k v))))
        r))
    (define (bag-xor! b1 b2)
      (let ((to-del '()) (to-add '()))
        (hash-table-walk (bag-ht b1)
          (lambda (k v)
            (when (hash-table-exists? (bag-ht b2) k)
              (set! to-del (cons k to-del)))))
        (hash-table-walk (bag-ht b2)
          (lambda (k v)
            (unless (hash-table-exists? (bag-ht b1) k)
              (set! to-add (cons (cons k v) to-add)))))
        (for-each (lambda (k) (hash-table-delete! (bag-ht b1) k)) to-del)
        (for-each (lambda (kv) (hash-table-set! (bag-ht b1) (car kv) (cdr kv))) to-add))
      b1)

    (define (bag=? b1 b2)
      (and (= (hash-table-size (bag-ht b1)) (hash-table-size (bag-ht b2)))
           (every (lambda (k)
             (= (bag-element-count b1 k) (bag-element-count b2 k)))
             (hash-table-keys (bag-ht b1)))))
    (define (bag<? b1 b2)
      (and (< (bag-size b1) (bag-size b2))
           (every (lambda (k) (<= (bag-element-count b1 k) (bag-element-count b2 k)))
                  (hash-table-keys (bag-ht b1)))))
    (define (bag>? b1 b2) (bag<? b2 b1))
    (define (bag<=? b1 b2)
      (every (lambda (k) (<= (bag-element-count b1 k) (bag-element-count b2 k)))
             (hash-table-keys (bag-ht b1))))
    (define (bag>=? b1 b2) (bag<=? b2 b1))

    (define (bag-for-each-unique proc b)
      (hash-table-walk (bag-ht b) (lambda (k v) (proc k v))))

    (define (bag-fold-unique proc nil b)
      (hash-table-fold (bag-ht b) (lambda (k v acc) (proc k v acc)) nil))

    (define (bag-increment! b e n)
      (let ((cur (bag-element-count b e)))
        (hash-table-set! (bag-ht b) e (+ cur n)))
      b)
    (define (bag-decrement! b e n)
      (let ((cur (bag-element-count b e)))
        (if (<= cur n)
            (hash-table-delete! (bag-ht b) e)
            (hash-table-set! (bag-ht b) e (- cur n))))
      b)

    (define (bag->set b)
      (let ((s (%make-set (make-set-ht (bag-comparator b)) (bag-comparator b))))
        (hash-table-walk (bag-ht b)
          (lambda (k v) (hash-table-set! (set-ht s) k #t)))
        s))
    (define (set->bag s)
      (let ((b (%make-bag (make-bag-ht (set-comparator s)) (set-comparator s))))
        (hash-table-walk (set-ht s)
          (lambda (k v) (hash-table-set! (bag-ht b) k 1)))
        b))
    (define (bag->alist b)
      (hash-table-fold (bag-ht b)
        (lambda (k v acc) (cons (cons k v) acc)) '()))
    (define (alist->bag cmp alist)
      (let ((b (%make-bag (make-bag-ht cmp) cmp)))
        (for-each (lambda (p)
          (hash-table-set! (bag-ht b) (car p) (cdr p)))
          alist)
        b))))
|});

  (* SRFI 1 — list library *)
  (["srfi"; "1"], {|
(define-library (srfi 1)
  (import (scheme base) (scheme case-lambda))
  (export
    ;; constructors
    xcons cons* make-list list-tabulate list-copy circular-list iota
    ;; predicates
    proper-list? dotted-list? circular-list? null-list? not-pair? list=
    ;; selectors
    first second third fourth fifth sixth seventh eighth ninth tenth
    car+cdr take drop take-right drop-right split-at last last-pair
    ;; fold / unfold / map
    fold fold-right unfold unfold-right
    append-map filter-map pair-for-each map-in-order
    ;; filtering
    filter partition remove
    ;; searching
    find find-tail any every list-index
    take-while drop-while span break
    ;; deleting
    delete delete-duplicates
    ;; alists
    alist-cons alist-copy alist-delete
    ;; set operations
    lset-union lset-intersection lset-difference lset-xor lset-adjoin
    ;; misc
    zip unzip1 unzip2 concatenate append-reverse count)
  (begin
    ;; constructors
    (define (xcons d a) (cons a d))
    (define (cons* first . rest)
      (if (null? rest) first
          (cons first (apply cons* rest))))
    (define make-list
      (case-lambda
        ((n) (make-list n #f))
        ((n fill)
         (let loop ((i 0) (acc '()))
           (if (>= i n) acc (loop (+ i 1) (cons fill acc)))))))
    (define (list-tabulate n proc)
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc (loop (- i 1) (cons (proc i) acc)))))
    (define (list-copy lst)
      (if (pair? lst)
          (cons (car lst) (list-copy (cdr lst)))
          lst))
    (define (circular-list . elts)
      (let ((lst (list-copy elts)))
        (if (null? lst) (error "circular-list: empty")
            (begin (set-cdr! (last-pair lst) lst) lst))))
    (define iota
      (case-lambda
        ((count) (iota count 0 1))
        ((count start) (iota count start 1))
        ((count start step)
         (let loop ((i (- count 1)) (acc '()))
           (if (< i 0) acc
               (loop (- i 1) (cons (+ start (* i step)) acc)))))))

    ;; predicates
    (define (proper-list? x)
      (let loop ((fast x) (slow x))
        (cond
          ((null? fast) #t)
          ((not (pair? fast)) #f)
          ((null? (cdr fast)) #t)
          ((not (pair? (cdr fast))) #f)
          ((eq? (cdr fast) slow) #f)
          (else (loop (cddr fast) (cdr slow))))))
    (define (dotted-list? x)
      (cond
        ((null? x) #f)
        ((not (pair? x)) #t)
        (else (let loop ((fast (cdr x)) (slow x))
                (cond
                  ((null? fast) #f)
                  ((not (pair? fast)) #t)
                  ((null? (cdr fast)) #f)
                  ((not (pair? (cdr fast))) #t)
                  ((eq? (cdr fast) slow) #f)
                  (else (loop (cddr fast) (cdr slow))))))))
    (define (circular-list? x)
      (and (pair? x)
           (let loop ((fast (cdr x)) (slow x))
             (cond
               ((null? fast) #f)
               ((not (pair? fast)) #f)
               ((null? (cdr fast)) #f)
               ((not (pair? (cdr fast))) #f)
               ((eq? (cdr fast) slow) #t)
               (else (loop (cddr fast) (cdr slow)))))))
    (define (null-list? x)
      (cond ((null? x) #t) ((pair? x) #f)
            (else (error "null-list?: not a list" x))))
    (define (not-pair? x) (not (pair? x)))
    (define (list= elt= . lists)
      (or (null? lists)
          (null? (cdr lists))
          (let loop ((a (car lists)) (b (cadr lists)))
            (cond
              ((and (null? a) (null? b)) (apply list= elt= (cdr lists)))
              ((or (null? a) (null? b)) #f)
              ((elt= (car a) (car b)) (loop (cdr a) (cdr b)))
              (else #f)))))

    ;; selectors
    (define (first x) (car x))
    (define (second x) (cadr x))
    (define (third x) (caddr x))
    (define (fourth x) (cadddr x))
    (define (fifth x) (car (cddddr x)))
    (define (sixth x) (cadr (cddddr x)))
    (define (seventh x) (caddr (cddddr x)))
    (define (eighth x) (cadddr (cddddr x)))
    (define (ninth x) (car (cddddr (cddddr x))))
    (define (tenth x) (cadr (cddddr (cddddr x))))
    (define (car+cdr x) (values (car x) (cdr x)))
    (define (take lst k)
      (let loop ((lst lst) (k k) (acc '()))
        (if (= k 0) (reverse acc)
            (loop (cdr lst) (- k 1) (cons (car lst) acc)))))
    (define (drop lst k)
      (if (= k 0) lst (drop (cdr lst) (- k 1))))
    (define (take-right lst k)
      (let ((len (length lst)))
        (drop lst (- len k))))
    (define (drop-right lst k)
      (take lst (- (length lst) k)))
    (define (split-at lst k)
      (values (take lst k) (drop lst k)))
    (define (last lst)
      (if (null? (cdr lst)) (car lst) (last (cdr lst))))
    (define (last-pair lst)
      (if (null? (cdr lst)) lst (last-pair (cdr lst))))

    ;; fold / unfold / map
    (define (fold kons knil lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst) (acc knil))
            (if (null? lst) acc
                (loop (cdr lst) (kons (car lst) acc))))
          (let loop ((lsts (cons lst lsts)) (acc knil))
            (if (any null? lsts) acc
                (loop (map cdr lsts)
                      (apply kons (append (map car lsts) (list acc))))))))
    (define (fold-right kons knil lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst))
            (if (null? lst) knil
                (kons (car lst) (loop (cdr lst)))))
          (let loop ((lsts (cons lst lsts)))
            (if (any null? lsts) knil
                (apply kons
                  (append (map car lsts)
                          (list (loop (map cdr lsts)))))))))
    (define (unfold p f g seed . maybe-tail)
      (let ((tail-gen (if (null? maybe-tail)
                          (lambda (x) '())
                          (car maybe-tail))))
        (let loop ((seed seed))
          (if (p seed) (tail-gen seed)
              (cons (f seed) (loop (g seed)))))))
    (define (unfold-right p f g seed . maybe-tail)
      (let ((tail (if (null? maybe-tail) '() (car maybe-tail))))
        (let loop ((seed seed) (acc tail))
          (if (p seed) acc
              (loop (g seed) (cons (f seed) acc))))))
    (define (append-map f lst . lsts)
      (if (null? lsts)
          (apply append (map f lst))
          (let loop ((lsts (cons lst lsts)) (acc '()))
            (if (any null? lsts)
                (apply append (reverse acc))
                (loop (map cdr lsts)
                      (cons (apply f (map car lsts)) acc))))))
    (define (filter-map f lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst) (acc '()))
            (cond
              ((null? lst) (reverse acc))
              (else
               (let ((v (f (car lst))))
                 (if v
                     (loop (cdr lst) (cons v acc))
                     (loop (cdr lst) acc))))))
          (let loop ((lsts (cons lst lsts)) (acc '()))
            (if (any null? lsts) (reverse acc)
                (let ((v (apply f (map car lsts))))
                  (loop (map cdr lsts)
                        (if v (cons v acc) acc)))))))
    (define (pair-for-each f lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst))
            (unless (null? lst)
              (f lst) (loop (cdr lst))))
          (let loop ((lsts (cons lst lsts)))
            (unless (any null? lsts)
              (apply f lsts) (loop (map cdr lsts))))))
    (define (map-in-order f lst . lsts)
      (if (null? lsts)
          (map f lst)
          (let loop ((lsts (cons lst lsts)) (acc '()))
            (if (any null? lsts) (reverse acc)
                (loop (map cdr lsts)
                      (cons (apply f (map car lsts)) acc))))))

    ;; filtering
    (define (filter pred lst)
      (let loop ((lst lst) (acc '()))
        (cond
          ((null? lst) (reverse acc))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc)))
          (else (loop (cdr lst) acc)))))
    (define (partition pred lst)
      (let loop ((lst lst) (yes '()) (no '()))
        (cond
          ((null? lst) (values (reverse yes) (reverse no)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no))
          (else (loop (cdr lst) yes (cons (car lst) no))))))
    (define (remove pred lst)
      (filter (lambda (x) (not (pred x))) lst))

    ;; searching
    (define (find pred lst)
      (cond
        ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find pred (cdr lst)))))
    (define (find-tail pred lst)
      (cond
        ((null? lst) #f)
        ((pred (car lst)) lst)
        (else (find-tail pred (cdr lst)))))
    (define (any pred lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst))
            (if (null? lst) #f
                (let ((v (pred (car lst))))
                  (if v v (loop (cdr lst))))))
          (let loop ((lsts (cons lst lsts)))
            (if (any null? lsts) #f
                (let ((v (apply pred (map car lsts))))
                  (if v v (loop (map cdr lsts))))))))
    (define (every pred lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst))
            (cond
              ((null? lst) #t)
              ((null? (cdr lst)) (pred (car lst)))
              ((pred (car lst)) (loop (cdr lst)))
              (else #f)))
          (let loop ((lsts (cons lst lsts)))
            (cond
              ((any null? lsts) #t)
              ((any (lambda (l) (null? (cdr l))) lsts)
               (apply pred (map car lsts)))
              ((apply pred (map car lsts)) (loop (map cdr lsts)))
              (else #f)))))
    (define (list-index pred lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst) (i 0))
            (cond
              ((null? lst) #f)
              ((pred (car lst)) i)
              (else (loop (cdr lst) (+ i 1)))))
          (let loop ((lsts (cons lst lsts)) (i 0))
            (cond
              ((any null? lsts) #f)
              ((apply pred (map car lsts)) i)
              (else (loop (map cdr lsts) (+ i 1)))))))
    (define (take-while pred lst)
      (let loop ((lst lst) (acc '()))
        (if (or (null? lst) (not (pred (car lst))))
            (reverse acc)
            (loop (cdr lst) (cons (car lst) acc)))))
    (define (drop-while pred lst)
      (let loop ((lst lst))
        (if (or (null? lst) (not (pred (car lst))))
            lst
            (loop (cdr lst)))))
    (define (span pred lst)
      (values (take-while pred lst) (drop-while pred lst)))
    (define (break pred lst)
      (span (lambda (x) (not (pred x))) lst))

    ;; deleting
    (define delete
      (case-lambda
        ((x lst) (delete x lst equal?))
        ((x lst =)
         (filter (lambda (e) (not (= e x))) lst))))
    (define delete-duplicates
      (case-lambda
        ((lst) (delete-duplicates lst equal?))
        ((lst =)
         (let loop ((lst lst) (acc '()))
           (cond
             ((null? lst) (reverse acc))
             ((any (lambda (e) (= (car lst) e)) acc)
              (loop (cdr lst) acc))
             (else (loop (cdr lst) (cons (car lst) acc))))))))

    ;; alists
    (define (alist-cons key datum alist) (cons (cons key datum) alist))
    (define (alist-copy alist) (map (lambda (p) (cons (car p) (cdr p))) alist))
    (define alist-delete
      (case-lambda
        ((key alist) (alist-delete key alist equal?))
        ((key alist =)
         (filter (lambda (p) (not (= (car p) key))) alist))))

    ;; set operations
    (define (lset-adjoin = set . elts)
      (fold (lambda (elt set)
              (if (any (lambda (s) (= s elt)) set)
                  set
                  (cons elt set)))
            set elts))
    (define (lset-union = . lists)
      (fold (lambda (list result)
              (fold (lambda (elt result)
                      (if (any (lambda (r) (= r elt)) result)
                          result
                          (cons elt result)))
                    result list))
            '() lists))
    (define (lset-intersection = list1 . lists)
      (filter (lambda (elt)
                (every (lambda (list)
                         (any (lambda (e) (= e elt)) list))
                       lists))
              list1))
    (define (lset-difference = list1 . lists)
      (filter (lambda (elt)
                (not (any (lambda (list)
                            (any (lambda (e) (= e elt)) list))
                          lists)))
              list1))
    (define (lset-xor = . lists)
      (fold (lambda (list result)
              (let ((in-result (filter (lambda (e)
                                         (any (lambda (r) (= r e)) result))
                                       list))
                    (not-in-result (filter (lambda (e)
                                             (not (any (lambda (r) (= r e)) result)))
                                           list)))
                (append (filter (lambda (r)
                                  (not (any (lambda (e) (= e r)) in-result)))
                                result)
                        not-in-result)))
            '() lists))

    ;; misc
    (define (zip . lists)
      (if (any null? lists) '()
          (cons (map car lists)
                (apply zip (map cdr lists)))))
    (define (unzip1 lists) (map car lists))
    (define (unzip2 lists) (values (map car lists) (map cadr lists)))
    (define (concatenate lists) (apply append lists))
    (define (append-reverse rev-head tail)
      (fold cons tail rev-head))
    (define (count pred lst . lsts)
      (if (null? lsts)
          (let loop ((lst lst) (n 0))
            (if (null? lst) n
                (loop (cdr lst) (if (pred (car lst)) (+ n 1) n))))
          (let loop ((lsts (cons lst lsts)) (n 0))
            (if (any null? lsts) n
                (loop (map cdr lsts)
                      (if (apply pred (map car lsts)) (+ n 1) n))))))))
|});

  (* SRFI 145 — assumptions *)
  (["srfi"; "145"], {|
(define-library (srfi 145)
  (import (scheme base))
  (export assume)
  (begin
    (define-syntax assume
      (syntax-rules ()
        ((assume expression message ...)
         (or expression
             (error "invalid assumption" (quote expression) message ...)))
        ((assume . _)
         (syntax-error "invalid assume syntax"))))))
|});

  (* SRFI 156 — syntactic combiners for binary predicates *)
  (["srfi"; "156"], {|
(define-library (srfi 156)
  (import (scheme base))
  (export is isnt infix/postfix extract-placeholders identity-syntax)
  (begin
    (define-syntax infix/postfix
      (syntax-rules ()
        ((infix/postfix x somewhat?)
         (somewhat? x))
        ((infix/postfix left related-to? right)
         (related-to? left right))
        ((infix/postfix left related-to? right . likewise)
         (let ((right* right))
           (and (infix/postfix left related-to? right*)
                (infix/postfix right* . likewise))))))

    (define-syntax extract-placeholders
      (syntax-rules (_)
        ((extract-placeholders final () () body)
         (final (infix/postfix . body)))
        ((extract-placeholders final () args body)
         (lambda args (final (infix/postfix . body))))
        ((extract-placeholders final (_ op . rest) (args ...) (body ...))
         (extract-placeholders final rest (args ... arg) (body ... arg op)))
        ((extract-placeholders final (arg op . rest) args (body ...))
         (extract-placeholders final rest args (body ... arg op)))
        ((extract-placeholders final (_) (args ...) (body ...))
         (extract-placeholders final () (args ... arg) (body ... arg)))
        ((extract-placeholders final (arg) args (body ...))
         (extract-placeholders final () args (body ... arg)))))

    (define-syntax identity-syntax
      (syntax-rules ()
        ((identity-syntax form) form)))

    (define-syntax is
      (syntax-rules ()
        ((is . something)
         (extract-placeholders identity-syntax something () ()))))

    (define-syntax isnt
      (syntax-rules ()
        ((isnt . something)
         (extract-placeholders not something () ()))))))
|});

  (* SRFI 219 — define higher-order lambda
     Note: The original SRFI shadows core 'define'. Since Wile's expander
     treats 'define' as a core form that cannot be shadowed by syntax-rules,
     we export 'define-curried' as the user-facing name. *)
  (["srfi"; "219"], {|
(define-library (srfi 219)
  (import (scheme base))
  (export define-curried)
  (begin
    (define-syntax define-curried
      (syntax-rules ()
        ((define-curried ((head . inner-args) . outer-args) body ...)
         (define-curried (head . inner-args)
           (lambda outer-args body ...)))
        ((define-curried (name . args) body ...)
         (define name (lambda args body ...)))
        ((define-curried name expr)
         (define name expr))))))
|});

  (* SRFI 223 — generalized binary search *)
  (["srfi"; "223"], {|
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
|});

  (* SRFI 175 — ASCII character library *)
  (["srfi"; "175"], {|
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
|});

  (* SRFI 162 — comparators sublibrary *)
  (["srfi"; "162"], {|
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
|});

  (* SRFI 228 — composing comparators *)
  (["srfi"; "228"], {|
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
|});

  (* SRFI 195 — multiple-value boxes *)
  (["srfi"; "195"], {|
(define-library (srfi 195)
  (import (scheme base))
  (export box box? unbox set-box! box-arity unbox-value set-box-value!)
  (begin
    (define-record-type <mv-box>
      (%make-box vals)
      box?
      (vals %box-vals %set-box-vals!))

    (define (box . vals) (%make-box (list->vector vals)))

    (define (unbox b) (apply values (vector->list (%box-vals b))))

    (define (set-box! b . vals) (%set-box-vals! b (list->vector vals)))

    (define (box-arity b) (vector-length (%box-vals b)))

    (define (unbox-value b i)
      (vector-ref (%box-vals b) i))

    (define (set-box-value! b i v)
      (vector-set! (%box-vals b) i v))))
|});

  (* SRFI 48 — intermediate format strings *)
  (["srfi"; "48"], {|
(define-library (srfi 48)
  (import (scheme base) (scheme write) (scheme char))
  (export format)
  (begin
    (define (format . args)
      (let* ((has-port (or (boolean? (car args))
                           (and (not (string? (car args)))
                                (not (null? args)))))
             (port (cond ((and has-port (eq? (car args) #t))
                          (current-output-port))
                         ((and has-port (eq? (car args) #f))
                          (open-output-string))
                         (has-port (car args))
                         (else (open-output-string))))
             (template (if has-port (cadr args) (car args)))
             (fargs (if has-port (cddr args) (cdr args)))
             (return-string (or (not has-port)
                                (and has-port (eq? (car args) #f)))))
        (let ((len (string-length template)))
          (let loop ((i 0) (args fargs))
            (cond
              ((>= i len)
               (if return-string (get-output-string port) (if #f #f)))
              ((and (char=? (string-ref template i) #\~)
                    (< (+ i 1) len))
               (let ((c (string-ref template (+ i 1))))
                 (cond
                   ((or (char=? c #\a) (char=? c #\A))
                    (if (null? args) (error "format: not enough arguments"))
                    (display (car args) port)
                    (loop (+ i 2) (cdr args)))
                   ((or (char=? c #\s) (char=? c #\S))
                    (if (null? args) (error "format: not enough arguments"))
                    (write (car args) port)
                    (loop (+ i 2) (cdr args)))
                   ((char=? c #\%)
                    (newline port)
                    (loop (+ i 2) args))
                   ((char=? c #\~)
                    (write-char #\~ port)
                    (loop (+ i 2) args))
                   ((or (char=? c #\d) (char=? c #\D))
                    (if (null? args) (error "format: not enough arguments"))
                    (display (number->string (car args)) port)
                    (loop (+ i 2) (cdr args)))
                   ((or (char=? c #\b) (char=? c #\B))
                    (if (null? args) (error "format: not enough arguments"))
                    (display (number->string (car args) 2) port)
                    (loop (+ i 2) (cdr args)))
                   ((or (char=? c #\o) (char=? c #\O))
                    (if (null? args) (error "format: not enough arguments"))
                    (display (number->string (car args) 8) port)
                    (loop (+ i 2) (cdr args)))
                   ((or (char=? c #\x) (char=? c #\X))
                    (if (null? args) (error "format: not enough arguments"))
                    (display (number->string (car args) 16) port)
                    (loop (+ i 2) (cdr args)))
                   ((or (char=? c #\c) (char=? c #\C))
                    (if (null? args) (error "format: not enough arguments"))
                    (write-char (car args) port)
                    (loop (+ i 2) (cdr args)))
                   ((char=? c #\!)
                    (flush-output-port port)
                    (loop (+ i 2) args))
                   (else
                    (error "format: unknown escape" c)))))
              (else
               (write-char (string-ref template i) port)
               (loop (+ i 1) args)))))))))
|});

  (* SRFI 117 — queues based on lists *)
  (["srfi"; "117"], {|
(define-library (srfi 117)
  (import (scheme base))
  (export
    make-list-queue list-queue list-queue? list-queue-empty?
    list-queue-front list-queue-back
    list-queue-list list-queue-first-last
    list-queue-add-front! list-queue-add-back!
    list-queue-remove-front! list-queue-remove-all!
    list-queue-set-list!
    list-queue-append list-queue-append! list-queue-concatenate
    list-queue-map list-queue-for-each
    list-queue-copy)
  (begin
    (define-record-type <list-queue>
      (%make-lq front back)
      list-queue?
      (front lq-front set-lq-front!)
      (back lq-back set-lq-back!))

    (define (make-list-queue lst . rest)
      (if (null? lst)
          (%make-lq '() '())
          (let ((last (if (null? rest)
                          (let loop ((p lst)) (if (null? (cdr p)) p (loop (cdr p))))
                          (car rest))))
            (%make-lq lst last))))

    (define (list-queue . elts)
      (make-list-queue elts))

    (define (list-queue-empty? q) (null? (lq-front q)))

    (define (list-queue-front q)
      (if (null? (lq-front q))
          (error "list-queue-front: empty queue")
          (car (lq-front q))))

    (define (list-queue-back q)
      (if (null? (lq-front q))
          (error "list-queue-back: empty queue")
          (car (lq-back q))))

    (define (list-queue-list q) (lq-front q))
    (define (list-queue-first-last q) (values (lq-front q) (lq-back q)))

    (define (list-queue-add-front! q elt)
      (let ((new (cons elt (lq-front q))))
        (if (null? (lq-front q))
            (begin (set-lq-front! q new)
                   (set-lq-back! q new))
            (set-lq-front! q new))))

    (define (list-queue-add-back! q elt)
      (let ((new (list elt)))
        (if (null? (lq-front q))
            (begin (set-lq-front! q new)
                   (set-lq-back! q new))
            (begin (set-cdr! (lq-back q) new)
                   (set-lq-back! q new)))))

    (define (list-queue-remove-front! q)
      (if (null? (lq-front q))
          (error "list-queue-remove-front!: empty queue")
          (let ((val (car (lq-front q))))
            (set-lq-front! q (cdr (lq-front q)))
            (when (null? (lq-front q))
              (set-lq-back! q '()))
            val)))

    (define (list-queue-remove-all! q)
      (let ((result (lq-front q)))
        (set-lq-front! q '())
        (set-lq-back! q '())
        result))

    (define (list-queue-set-list! q lst . rest)
      (set-lq-front! q lst)
      (if (null? rest)
          (if (null? lst)
              (set-lq-back! q '())
              (let loop ((p lst)) (if (null? (cdr p)) (set-lq-back! q p) (loop (cdr p)))))
          (set-lq-back! q (car rest))))

    (define (list-queue-copy q)
      (let ((lst (list-copy (lq-front q))))
        (make-list-queue lst)))

    (define (list-queue-append . qs)
      (let ((result (list-queue)))
        (for-each (lambda (q)
          (for-each (lambda (elt) (list-queue-add-back! result elt))
                    (lq-front q)))
          qs)
        result))

    (define (list-queue-append! q . qs)
      (for-each (lambda (q2)
        (for-each (lambda (elt) (list-queue-add-back! q elt))
                  (lq-front q2)))
        qs)
      q)

    (define (list-queue-concatenate qs)
      (apply list-queue-append qs))

    (define (list-queue-map proc q)
      (let ((result (list-queue)))
        (for-each (lambda (elt)
          (list-queue-add-back! result (proc elt)))
          (lq-front q))
        result))

    (define (list-queue-for-each proc q)
      (for-each proc (lq-front q)))))
|});

  (* SRFI 234 — topological sorting *)
  (["srfi"; "234"], {|
(define-library (srfi 234)
  (import (scheme base) (scheme case-lambda))
  (export topological-sort edgelist->graph
          edgelist/inverted->graph graph->edgelist
          graph->edgelist/inverted connected-components)
  (begin
    ;; A graph is ((node . (successor ...)) ...)
    (define (topological-sort graph . rest)
      (let ((eq (if (null? rest) equal? (car rest))))
        (let* ((nodes (map car graph))
               (in-degree (map (lambda (n) (cons n 0)) nodes)))
          ;; Count in-degrees
          (for-each (lambda (entry)
            (for-each (lambda (succ)
              (let ((p (%assoc succ in-degree eq)))
                (when p (set-cdr! p (+ (cdr p) 1)))))
              (cdr entry)))
            graph)
          ;; Collect zero in-degree nodes
          (let loop ((queue (filter (lambda (p) (= (cdr p) 0)) in-degree))
                     (result '())
                     (remaining (filter (lambda (p) (> (cdr p) 0)) in-degree)))
            (if (null? queue)
                (if (null? remaining)
                    (reverse result)
                    (error "topological-sort: cycle detected"))
                (let* ((node (caar queue))
                       (succs (let ((e (%assoc node graph eq)))
                                (if e (cdr e) '())))
                       (new-remaining
                         (map (lambda (p)
                           (if (%member (car p) succs eq)
                               (cons (car p) (- (cdr p) 1))
                               p))
                           remaining))
                       (new-queue (filter (lambda (p) (= (cdr p) 0)) new-remaining))
                       (still-remaining (filter (lambda (p) (> (cdr p) 0)) new-remaining)))
                  (loop (append (cdr queue) new-queue)
                        (cons node result)
                        still-remaining)))))))

    (define (filter pred lst)
      (let loop ((lst lst) (acc '()))
        (cond ((null? lst) (reverse acc))
              ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc)))
              (else (loop (cdr lst) acc)))))

    (define (%member x lst eq)
      (cond ((null? lst) #f)
            ((eq x (car lst)) lst)
            (else (%member x (cdr lst) eq))))

    (define (%assoc key alist eq)
      (cond ((null? alist) #f)
            ((eq key (caar alist)) (car alist))
            (else (%assoc key (cdr alist) eq))))

    (define (edgelist->graph edges . rest)
      (let ((eq (if (null? rest) equal? (car rest))))
        (let loop ((edges edges) (graph '()))
          (if (null? edges) graph
              (let* ((edge (car edges))
                     (from (car edge))
                     (to (cadr edge))
                     (entry (%assoc from graph eq)))
                (if entry
                    (begin (set-cdr! entry (cons to (cdr entry)))
                           (loop (cdr edges) graph))
                    (loop (cdr edges) (cons (list from to) graph))))))))

    (define (edgelist/inverted->graph edges . rest)
      (let ((eq (if (null? rest) equal? (car rest))))
        (apply edgelist->graph
          (map (lambda (e) (list (cadr e) (car e))) edges)
          rest)))

    (define (graph->edgelist graph)
      (let loop ((g graph) (acc '()))
        (if (null? g) acc
            (let ((node (caar g)) (succs (cdar g)))
              (loop (cdr g)
                    (append acc (map (lambda (s) (list node s)) succs)))))))

    (define (graph->edgelist/inverted graph)
      (map (lambda (e) (list (cadr e) (car e)))
           (graph->edgelist graph)))

    (define (connected-components graph . rest)
      (let ((eq (if (null? rest) equal? (car rest))))
        (let ((all-nodes (map car graph))
              (visited '())
              (components '()))
          (define (neighbors node)
            (let ((e (%assoc node graph eq)))
              (if e (cdr e) '())))
          (define (dfs node component)
            (unless (%member node visited eq)
              (set! visited (cons node visited))
              (set! component (cons node component))
              (for-each (lambda (n) (set! component (dfs n component)))
                        (neighbors node)))
            component)
          (for-each (lambda (node)
            (unless (%member node visited eq)
              (let ((comp (dfs node '())))
                (set! components (cons comp components)))))
            all-nodes)
          components)))))
|});

  (* SRFI 235 — combinators *)
  (["srfi"; "235"], {|
(define-library (srfi 235)
  (import (scheme base) (scheme case-lambda))
  (export
    constantly complement swap flip on-left on-right
    conjoin disjoin each-of all-of any-of on
    left-section right-section apply-chain
    arguments-drop arguments-drop-right
    arguments-take arguments-take-right
    group-by begin-procedure if-procedure when-procedure
    unless-procedure value-procedure case-procedure
    and-procedure eager-and-procedure
    or-procedure eager-or-procedure
    funcall-procedure loop-procedure while-procedure until-procedure
    always never boolean)
  (begin
    (define (constantly . vals)
      (lambda args (apply values vals)))

    (define (complement proc)
      (lambda args (not (apply proc args))))

    (define (swap proc)
      (lambda (a b . rest) (apply proc b a rest)))

    (define (flip proc)
      (lambda args (apply proc (reverse args))))

    (define (on-left proc) (lambda (a b) (proc a)))
    (define (on-right proc) (lambda (a b) (proc b)))

    (define (conjoin . preds)
      (lambda args
        (let loop ((ps preds))
          (cond ((null? ps) #t)
                ((null? (cdr ps)) (apply (car ps) args))
                ((apply (car ps) args) (loop (cdr ps)))
                (else #f)))))

    (define (disjoin . preds)
      (lambda args
        (let loop ((ps preds))
          (cond ((null? ps) #f)
                ((null? (cdr ps)) (apply (car ps) args))
                (else (let ((v (apply (car ps) args)))
                        (if v v (loop (cdr ps)))))))))

    (define (each-of . procs)
      (lambda args
        (for-each (lambda (p) (apply p args)) procs)))

    (define (all-of pred)
      (lambda (lst)
        (let loop ((lst lst))
          (cond ((null? lst) #t)
                ((null? (cdr lst)) (pred (car lst)))
                ((pred (car lst)) (loop (cdr lst)))
                (else #f)))))

    (define (any-of pred)
      (lambda (lst)
        (let loop ((lst lst))
          (if (null? lst) #f
              (let ((v (pred (car lst))))
                (if v v (loop (cdr lst))))))))

    (define (on proc . procs)
      (lambda args
        (define (zip-apply ps as)
          (if (or (null? ps) (null? as)) '()
              (cons ((car ps) (car as))
                    (zip-apply (cdr ps) (cdr as)))))
        (apply proc (zip-apply procs args))))

    (define (left-section proc . bound-args)
      (lambda args (apply proc (append bound-args args))))

    (define (right-section proc . bound-args)
      (lambda args (apply proc (append args bound-args))))

    (define (apply-chain . procs)
      (lambda args
        (let loop ((ps (reverse procs)) (vals args))
          (if (null? ps)
              (apply values vals)
              (loop (cdr ps)
                    (call-with-values
                      (lambda () (apply (car ps) vals))
                      list))))))

    (define (arguments-drop proc n)
      (lambda args (apply proc (list-tail args n))))

    (define (arguments-drop-right proc n)
      (lambda args
        (let ((len (length args)))
          (apply proc (take-list args (- len n))))))

    (define (arguments-take proc n)
      (lambda args (apply proc (take-list args n))))

    (define (arguments-take-right proc n)
      (lambda args
        (let ((len (length args)))
          (apply proc (list-tail args (- len n))))))

    (define (take-list lst n)
      (if (= n 0) '()
          (cons (car lst) (take-list (cdr lst) (- n 1)))))

    (define (group-by key . rest)
      (let ((= (if (null? rest) equal? (car rest))))
        (lambda (lst)
          (let loop ((lst lst) (groups '()))
            (if (null? lst)
                (reverse (map (lambda (g) (reverse (cdr g))) groups))
                (let* ((elt (car lst))
                       (k (key elt))
                       (found (let scan ((gs groups))
                                (cond ((null? gs) #f)
                                      ((= (caar gs) k) (car gs))
                                      (else (scan (cdr gs)))))))
                  (if found
                      (begin (set-cdr! found (cons elt (cdr found)))
                             (loop (cdr lst) groups))
                      (loop (cdr lst) (cons (list k elt) groups)))))))))

    (define (begin-procedure . thunks)
      (let loop ((ts thunks))
        (if (null? (cdr ts))
            ((car ts))
            (begin ((car ts)) (loop (cdr ts))))))

    (define (if-procedure val then-thunk else-thunk)
      (if val (then-thunk) (else-thunk)))

    (define (when-procedure val . thunks)
      (when val (for-each (lambda (t) (t)) thunks)))

    (define (unless-procedure val . thunks)
      (unless val (for-each (lambda (t) (t)) thunks)))

    (define (value-procedure val then-proc else-thunk)
      (if val (then-proc val) (else-thunk)))

    (define (case-procedure val . clauses)
      (let loop ((cs clauses))
        (cond ((null? cs) (if #f #f))
              ((null? (cdr cs)) ((car cs) val))
              (((car cs) val) => (lambda (r) r))
              (else (loop (cdr cs))))))

    (define (and-procedure . thunks)
      (let loop ((ts thunks))
        (cond ((null? ts) #t)
              ((null? (cdr ts)) ((car ts)))
              (((car ts)) (loop (cdr ts)))
              (else #f))))

    (define (eager-and-procedure . vals)
      (let loop ((vs vals))
        (cond ((null? vs) #t)
              ((null? (cdr vs)) (car vs))
              ((car vs) (loop (cdr vs)))
              (else #f))))

    (define (or-procedure . thunks)
      (let loop ((ts thunks))
        (cond ((null? ts) #f)
              ((null? (cdr ts)) ((car ts)))
              (else (let ((v ((car ts))))
                      (if v v (loop (cdr ts))))))))

    (define (eager-or-procedure . vals)
      (let loop ((vs vals))
        (cond ((null? vs) #f)
              ((null? (cdr vs)) (car vs))
              (else (let ((v (car vs)))
                      (if v v (loop (cdr vs))))))))

    (define (funcall-procedure proc . args)
      (apply proc args))

    (define (loop-procedure thunk)
      (let loop () (thunk) (loop)))

    (define (while-procedure thunk)
      (let loop ()
        (when (thunk) (loop))))

    (define (until-procedure thunk)
      (let loop ()
        (unless (thunk) (loop))))

    (define (always . args) #t)
    (define (never . args) #f)
    (define (boolean x) (if x #t #f))))
|});

  (* SRFI 158 — generators and accumulators *)
  (["srfi"; "158"], {|
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
|});

  (* SRFI 214 — flexvectors *)
  (["srfi"; "214"], {|
(define-library (srfi 214)
  (import (scheme base) (scheme case-lambda))
  (export
    flexvector? flexvector-length flexvector-ref flexvector-set!
    make-flexvector flexvector
    flexvector-add! flexvector-add-front! flexvector-add-back!
    flexvector-remove! flexvector-remove-front! flexvector-remove-back!
    flexvector-empty? flexvector->vector vector->flexvector
    flexvector->list list->flexvector flexvector-copy
    flexvector-append flexvector-append!
    flexvector-map flexvector-map! flexvector-for-each
    flexvector-filter flexvector-filter!
    flexvector-fold flexvector-fold-right
    flexvector-count flexvector-index flexvector-any flexvector-every
    flexvector-clear! flexvector-fill! flexvector-reverse!
    flexvector-swap!)
  (begin
    (define-record-type <flexvector>
      (%make-fv storage len)
      flexvector?
      (storage fv-storage set-fv-storage!)
      (len fv-len set-fv-len!))

    (define make-flexvector
      (case-lambda
        ((size) (make-flexvector size 0))
        ((size fill)
         (let ((fv (%make-fv (make-vector (max size 4) fill) size)))
           fv))))

    (define (flexvector . elts)
      (list->flexvector elts))

    (define (flexvector-length fv) (fv-len fv))

    (define (flexvector-ref fv i)
      (if (or (< i 0) (>= i (fv-len fv)))
          (error "flexvector-ref: index out of range" i)
          (vector-ref (fv-storage fv) i)))

    (define (flexvector-set! fv i val)
      (if (or (< i 0) (>= i (fv-len fv)))
          (error "flexvector-set!: index out of range" i)
          (vector-set! (fv-storage fv) i val)))

    (define (fv-grow! fv needed)
      (let ((stor (fv-storage fv)))
        (when (> needed (vector-length stor))
          (let* ((new-cap (max needed (* 2 (vector-length stor))))
                 (new-stor (make-vector new-cap)))
            (let loop ((i 0))
              (when (< i (fv-len fv))
                (vector-set! new-stor i (vector-ref stor i))
                (loop (+ i 1))))
            (set-fv-storage! fv new-stor)))))

    (define (flexvector-add! fv i . vals)
      (let ((n (length vals)))
        (fv-grow! fv (+ (fv-len fv) n))
        ;; Shift elements right
        (let ((stor (fv-storage fv)))
          (let loop ((j (- (+ (fv-len fv) n) 1)))
            (when (>= j (+ i n))
              (vector-set! stor j (vector-ref stor (- j n)))
              (loop (- j 1))))
          ;; Insert
          (let loop ((j i) (vs vals))
            (unless (null? vs)
              (vector-set! stor j (car vs))
              (loop (+ j 1) (cdr vs))))
          (set-fv-len! fv (+ (fv-len fv) n)))))

    (define (flexvector-add-front! fv . vals)
      (apply flexvector-add! fv 0 vals))

    (define (flexvector-add-back! fv . vals)
      (apply flexvector-add! fv (fv-len fv) vals))

    (define (flexvector-remove! fv i)
      (let ((val (flexvector-ref fv i))
            (stor (fv-storage fv)))
        (let loop ((j i))
          (when (< j (- (fv-len fv) 1))
            (vector-set! stor j (vector-ref stor (+ j 1)))
            (loop (+ j 1))))
        (set-fv-len! fv (- (fv-len fv) 1))
        val))

    (define (flexvector-remove-front! fv) (flexvector-remove! fv 0))
    (define (flexvector-remove-back! fv) (flexvector-remove! fv (- (fv-len fv) 1)))

    (define (flexvector-empty? fv) (= (fv-len fv) 0))

    (define (flexvector->vector fv)
      (let ((v (make-vector (fv-len fv))))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (vector-set! v i (vector-ref (fv-storage fv) i))
            (loop (+ i 1))))
        v))

    (define (vector->flexvector vec)
      (let* ((len (vector-length vec))
             (fv (%make-fv (make-vector (max len 4)) len)))
        (let loop ((i 0))
          (when (< i len)
            (vector-set! (fv-storage fv) i (vector-ref vec i))
            (loop (+ i 1))))
        fv))

    (define (flexvector->list fv)
      (let loop ((i (- (fv-len fv) 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (vector-ref (fv-storage fv) i) acc)))))

    (define (list->flexvector lst)
      (let ((fv (make-flexvector 0)))
        (for-each (lambda (x) (flexvector-add-back! fv x)) lst)
        fv))

    (define (flexvector-copy fv)
      (let ((nfv (%make-fv (vector-copy (fv-storage fv)) (fv-len fv))))
        nfv))

    (define (flexvector-append . fvs)
      (let ((result (make-flexvector 0)))
        (for-each (lambda (fv)
          (let loop ((i 0))
            (when (< i (fv-len fv))
              (flexvector-add-back! result (vector-ref (fv-storage fv) i))
              (loop (+ i 1)))))
          fvs)
        result))

    (define (flexvector-append! fv . fvs)
      (for-each (lambda (fv2)
        (let loop ((i 0))
          (when (< i (fv-len fv2))
            (flexvector-add-back! fv (vector-ref (fv-storage fv2) i))
            (loop (+ i 1)))))
        fvs)
      fv)

    (define (flexvector-map proc fv)
      (let ((result (make-flexvector (fv-len fv))))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (flexvector-set! result i (proc (vector-ref (fv-storage fv) i)))
            (loop (+ i 1))))
        result))

    (define (flexvector-map! proc fv)
      (let loop ((i 0))
        (when (< i (fv-len fv))
          (vector-set! (fv-storage fv) i (proc (vector-ref (fv-storage fv) i)))
          (loop (+ i 1)))))

    (define (flexvector-for-each proc fv)
      (let loop ((i 0))
        (when (< i (fv-len fv))
          (proc (vector-ref (fv-storage fv) i))
          (loop (+ i 1)))))

    (define (flexvector-filter pred fv)
      (let ((result (make-flexvector 0)))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (let ((v (vector-ref (fv-storage fv) i)))
              (when (pred v) (flexvector-add-back! result v)))
            (loop (+ i 1))))
        result))

    (define (flexvector-filter! pred fv)
      (let ((j 0))
        (let loop ((i 0))
          (when (< i (fv-len fv))
            (let ((v (vector-ref (fv-storage fv) i)))
              (when (pred v)
                (vector-set! (fv-storage fv) j v)
                (set! j (+ j 1))))
            (loop (+ i 1))))
        (set-fv-len! fv j)))

    (define (flexvector-fold proc seed fv)
      (let loop ((i 0) (acc seed))
        (if (>= i (fv-len fv)) acc
            (loop (+ i 1) (proc acc (vector-ref (fv-storage fv) i))))))

    (define (flexvector-fold-right proc seed fv)
      (let loop ((i (- (fv-len fv) 1)) (acc seed))
        (if (< i 0) acc
            (loop (- i 1) (proc acc (vector-ref (fv-storage fv) i))))))

    (define (flexvector-count pred fv)
      (flexvector-fold (lambda (n x) (if (pred x) (+ n 1) n)) 0 fv))

    (define (flexvector-index pred fv)
      (let loop ((i 0))
        (cond ((>= i (fv-len fv)) #f)
              ((pred (vector-ref (fv-storage fv) i)) i)
              (else (loop (+ i 1))))))

    (define (flexvector-any pred fv)
      (let loop ((i 0))
        (if (>= i (fv-len fv)) #f
            (let ((v (pred (vector-ref (fv-storage fv) i))))
              (if v v (loop (+ i 1)))))))

    (define (flexvector-every pred fv)
      (let loop ((i 0) (last #t))
        (if (>= i (fv-len fv)) last
            (let ((v (pred (vector-ref (fv-storage fv) i))))
              (if v (loop (+ i 1) v) #f)))))

    (define (flexvector-clear! fv)
      (set-fv-len! fv 0))

    (define (flexvector-fill! fv val)
      (let loop ((i 0))
        (when (< i (fv-len fv))
          (vector-set! (fv-storage fv) i val)
          (loop (+ i 1)))))

    (define (flexvector-reverse! fv)
      (let loop ((lo 0) (hi (- (fv-len fv) 1)))
        (when (< lo hi)
          (let ((tmp (vector-ref (fv-storage fv) lo)))
            (vector-set! (fv-storage fv) lo (vector-ref (fv-storage fv) hi))
            (vector-set! (fv-storage fv) hi tmp))
          (loop (+ lo 1) (- hi 1)))))

    (define (flexvector-swap! fv i j)
      (let ((tmp (flexvector-ref fv i)))
        (flexvector-set! fv i (flexvector-ref fv j))
        (flexvector-set! fv j tmp)))))
|});

  (* SRFI 189 — Maybe and Either *)
  (["srfi"; "189"], {|
(define-library (srfi 189)
  (import (scheme base) (scheme case-lambda))
  (export
    just nothing right left
    maybe? either? just? nothing? right? left?
    maybe-ref maybe-ref/default either-ref either-ref/default
    maybe-join either-join
    maybe-bind either-bind
    maybe-compose either-compose
    maybe-length either-length
    maybe-filter maybe-remove either-filter either-remove
    maybe-sequence either-sequence
    maybe->either either->maybe
    list->just list->right maybe->list either->list
    maybe->truth truth->maybe
    maybe->values values->maybe
    maybe-map maybe-fold maybe-unfold
    maybe-for-each either-map either-fold either-unfold
    either-for-each
    maybe-and maybe-or either-and either-or
    maybe-let* either-let*
    maybe-if
    maybe= either=
    either-swap
    exception->either
    tri-not tri=? tri-and tri-or tri-merge)
  (begin
    (define-record-type <just>
      (%raw-just objs)
      just?
      (objs just-objs))
    (define-record-type <nothing>
      (%make-nothing)
      nothing?)
    (define-record-type <left>
      (%raw-left objs)
      left?
      (objs left-objs))
    (define-record-type <right>
      (%raw-right objs)
      right?
      (objs right-objs))

    (define %nothing (%make-nothing))
    (define (nothing) %nothing)

    (define (just . objs) (%raw-just objs))
    (define (right . objs) (%raw-right objs))
    (define (left . objs) (%raw-left objs))

    (define (maybe? x) (or (just? x) (nothing? x)))
    (define (either? x) (or (left? x) (right? x)))

    (define maybe-ref
      (case-lambda
        ((m failure)
         (if (just? m)
             (apply values (just-objs m))
             (failure)))
        ((m failure success)
         (if (just? m)
             (apply success (just-objs m))
             (failure)))))

    (define (maybe-ref/default m . defaults)
      (if (just? m)
          (apply values (just-objs m))
          (apply values defaults)))

    (define either-ref
      (case-lambda
        ((e failure)
         (cond ((right? e) (apply values (right-objs e)))
               ((left? e) (apply failure (left-objs e)))
               (else (error "either-ref: not an either" e))))
        ((e failure success)
         (cond ((right? e) (apply success (right-objs e)))
               ((left? e) (apply failure (left-objs e)))
               (else (error "either-ref: not an either" e))))))

    (define (either-ref/default e . defaults)
      (if (right? e)
          (apply values (right-objs e))
          (apply values defaults)))

    (define (maybe-join m)
      (if (nothing? m) m
          (let ((inner (car (just-objs m))))
            inner)))

    (define (either-join e)
      (if (left? e) e
          (let ((inner (car (right-objs e))))
            inner)))

    (define (maybe-bind m . procs)
      (let loop ((m m) (ps procs))
        (cond ((null? ps) m)
              ((nothing? m) m)
              (else (loop (apply (car ps) (just-objs m)) (cdr ps))))))

    (define (either-bind e . procs)
      (let loop ((e e) (ps procs))
        (cond ((null? ps) e)
              ((left? e) e)
              (else (loop (apply (car ps) (right-objs e)) (cdr ps))))))

    (define (maybe-compose . procs)
      (lambda args
        (let loop ((m (apply just args)) (ps procs))
          (if (or (null? ps) (nothing? m)) m
              (loop (apply (car ps) (just-objs m)) (cdr ps))))))

    (define (either-compose . procs)
      (lambda args
        (let loop ((e (apply right args)) (ps procs))
          (if (or (null? ps) (left? e)) e
              (loop (apply (car ps) (right-objs e)) (cdr ps))))))

    (define (maybe-length m) (if (just? m) 1 0))
    (define (either-length e) (if (right? e) 1 0))

    (define (maybe-filter pred m)
      (if (nothing? m) m
          (if (apply pred (just-objs m)) m (nothing))))
    (define (maybe-remove pred m)
      (if (nothing? m) m
          (if (apply pred (just-objs m)) (nothing) m)))
    (define (either-filter pred e . rest)
      (if (left? e) e
          (if (apply pred (right-objs e)) e
              (apply left rest))))
    (define (either-remove pred e . rest)
      (if (left? e) e
          (if (apply pred (right-objs e))
              (apply left rest) e)))

    (define (maybe-sequence lst . rest)
      (let ((map-fn (if (null? rest) just (car rest))))
        (let loop ((lst lst) (acc '()))
          (cond ((null? lst) (map-fn (reverse acc)))
                ((nothing? (car lst)) (nothing))
                (else (loop (cdr lst)
                            (cons (car (just-objs (car lst))) acc)))))))

    (define (either-sequence lst . rest)
      (let ((map-fn (if (null? rest) right (car rest))))
        (let loop ((lst lst) (acc '()))
          (cond ((null? lst) (map-fn (reverse acc)))
                ((left? (car lst)) (car lst))
                (else (loop (cdr lst)
                            (cons (car (right-objs (car lst))) acc)))))))

    (define (maybe->either m . rest)
      (if (just? m) (apply right (just-objs m))
          (if (null? rest) (left 'nothing)
              (apply left rest))))

    (define (either->maybe e)
      (if (right? e) (apply just (right-objs e)) (nothing)))

    (define (list->just lst) (apply just lst))
    (define (list->right lst) (apply right lst))

    (define (maybe->list m)
      (if (nothing? m) '() (just-objs m)))

    (define (either->list e)
      (if (left? e) '() (right-objs e)))

    (define (maybe->truth m)
      (if (nothing? m) #f (car (just-objs m))))

    (define (truth->maybe x)
      (if x (just x) (nothing)))

    (define (maybe->values m)
      (if (nothing? m) (values)
          (apply values (just-objs m))))

    (define (values->maybe . objs)
      (if (null? objs) (nothing) (apply just objs)))

    (define (maybe-map proc m)
      (if (nothing? m) m
          (let ((result (apply proc (just-objs m))))
            (just result))))

    (define (maybe-fold proc seed m)
      (if (nothing? m) seed
          (apply proc seed (just-objs m))))

    (define (maybe-unfold stop? seed->maybe . rest)
      (if (stop? (if (null? rest) #f (car rest)))
          (nothing)
          (seed->maybe (if (null? rest) #f (car rest)))))

    (define (maybe-for-each proc m)
      (unless (nothing? m)
        (apply proc (just-objs m))))

    (define (either-map proc e)
      (if (left? e) e
          (let ((result (apply proc (right-objs e))))
            (right result))))

    (define (either-fold proc seed e)
      (if (left? e) seed
          (apply proc seed (right-objs e))))

    (define (either-unfold stop? seed->either . rest)
      (if (stop? (if (null? rest) #f (car rest)))
          (left 'unfold-stop)
          (seed->either (if (null? rest) #f (car rest)))))

    (define (either-for-each proc e)
      (when (right? e)
        (apply proc (right-objs e))))

    (define-syntax maybe-and
      (syntax-rules ()
        ((_) (just))
        ((_ m) m)
        ((_ m rest ...)
         (let ((t m))
           (if (just? t) (maybe-and rest ...) t)))))

    (define-syntax maybe-or
      (syntax-rules ()
        ((_) (nothing))
        ((_ m) m)
        ((_ m rest ...)
         (let ((t m))
           (if (just? t) t (maybe-or rest ...))))))

    (define-syntax either-and
      (syntax-rules ()
        ((_) (right))
        ((_ e) e)
        ((_ e rest ...)
         (let ((t e))
           (if (right? t) (either-and rest ...) t)))))

    (define-syntax either-or
      (syntax-rules ()
        ((_) (left))
        ((_ e) e)
        ((_ e rest ...)
         (let ((t e))
           (if (right? t) t (either-or rest ...))))))

    (define-syntax maybe-let*
      (syntax-rules ()
        ((_ () body ...) (begin body ...))
        ((_ ((var maybe-expr) rest ...) body ...)
         (let ((m maybe-expr))
           (if (nothing? m) m
               (let ((var (car (just-objs m))))
                 (maybe-let* (rest ...) body ...)))))
        ((_ ((maybe-expr) rest ...) body ...)
         (let ((m maybe-expr))
           (if (nothing? m) m
               (maybe-let* (rest ...) body ...))))))

    (define-syntax either-let*
      (syntax-rules ()
        ((_ () body ...) (begin body ...))
        ((_ ((var either-expr) rest ...) body ...)
         (let ((e either-expr))
           (if (left? e) e
               (let ((var (car (right-objs e))))
                 (either-let* (rest ...) body ...)))))
        ((_ ((either-expr) rest ...) body ...)
         (let ((e either-expr))
           (if (left? e) e
               (either-let* (rest ...) body ...))))))

    (define-syntax maybe-if
      (syntax-rules ()
        ((_ m just-expr nothing-expr)
         (if (just? m) just-expr nothing-expr))))

    (define (maybe= = m1 m2)
      (cond ((and (nothing? m1) (nothing? m2)) #t)
            ((or (nothing? m1) (nothing? m2)) #f)
            (else (apply = (append (just-objs m1) (just-objs m2))))))

    (define (either= = e1 e2)
      (cond ((and (left? e1) (left? e2))
             (apply = (append (left-objs e1) (left-objs e2))))
            ((and (right? e1) (right? e2))
             (apply = (append (right-objs e1) (right-objs e2))))
            (else #f)))

    (define (either-swap e)
      (cond ((right? e) (apply left (right-objs e)))
            ((left? e) (apply right (left-objs e)))
            (else (error "either-swap: not an either" e))))

    (define (exception->either pred thunk)
      (guard (exn ((pred exn) (left exn)))
        (call-with-values thunk right)))

    ;; Trivalent logic
    (define (tri-not x)
      (cond ((nothing? x) x)
            ((just? x) (if (car (just-objs x)) (just #f) (just #t)))
            (else (not x))))

    (define (tri=? m1 m2)
      (cond ((and (nothing? m1) (nothing? m2)) (just #t))
            ((or (nothing? m1) (nothing? m2)) (nothing))
            (else (just (equal? (just-objs m1) (just-objs m2))))))

    (define-syntax tri-and
      (syntax-rules ()
        ((_) (just #t))
        ((_ x) x)
        ((_ x rest ...)
         (let ((t x))
           (cond ((nothing? t) t)
                 ((and (just? t) (not (car (just-objs t)))) t)
                 (else (tri-and rest ...)))))))

    (define-syntax tri-or
      (syntax-rules ()
        ((_) (just #f))
        ((_ x) x)
        ((_ x rest ...)
         (let ((t x))
           (cond ((nothing? t) t)
                 ((and (just? t) (car (just-objs t))) t)
                 (else (tri-or rest ...)))))))

    (define (tri-merge . args)
      (let loop ((as args))
        (cond ((null? as) (nothing))
              ((nothing? (car as)) (loop (cdr as)))
              (else (car as)))))))
|});

  (* SRFI 210 — procedures for multiple values *)
  (["srfi"; "210"], {|
(define-library (srfi 210)
  (import (scheme base) (scheme case-lambda) (srfi 195))
  (export
    apply/mv call/mv list/mv vector/mv box/mv value/mv
    coarity with-values bind/mv
    list-values vector-values box-values value
    identity compose-left compose-right map-values
    bind/list bind/box bind
    set!-values)
  (begin
    (define (identity . args) (apply values args))

    (define (value . args)
      (if (= (length args) 1) (car args)
          (apply values args)))

    (define (list-values lst) (apply values lst))
    (define (vector-values vec) (apply values (vector->list vec)))
    (define (box-values b) (unbox b))

    (define (coarity thunk)
      (call-with-values thunk (lambda args (length args))))

    (define (list/mv . args)
      (let loop ((as args))
        (if (null? (cdr as))
            (call-with-values (lambda () (car as)) list)
            (cons (car as) (loop (cdr as))))))

    (define (vector/mv . args)
      (list->vector (apply list/mv args)))

    (define (box/mv . args)
      (apply box (apply list/mv args)))

    (define (value/mv . args)
      (apply values (apply list/mv args)))

    (define (apply/mv proc . args)
      (apply proc (apply list/mv args)))

    (define (call/mv consumer . producers)
      (apply consumer
        (apply append (map (lambda (p)
          (call-with-values p list)) producers))))

    (define (with-values producer consumer)
      (call-with-values producer consumer))

    (define (bind/mv producer . consumers)
      (let loop ((vals (call-with-values producer list))
                 (cs consumers))
        (if (null? cs)
            (apply values vals)
            (loop (call-with-values (lambda () (apply (car cs) vals)) list)
                  (cdr cs)))))

    (define compose-left
      (case-lambda
        (() identity)
        ((f) f)
        ((f . rest)
         (lambda args
           (let loop ((vals (call-with-values (lambda () (apply f args)) list))
                      (ps rest))
             (if (null? ps)
                 (apply values vals)
                 (loop (call-with-values (lambda () (apply (car ps) vals)) list)
                       (cdr ps))))))))

    (define compose-right
      (case-lambda
        (() identity)
        ((f) f)
        ((f . rest)
         (let ((chain (apply compose-left (reverse (cons f rest)))))
           chain))))

    (define (map-values proc)
      (lambda args (apply values (map proc args))))

    (define (bind/list proc lst) (apply proc lst))

    (define (bind/box proc b)
      (call-with-values (lambda () (unbox b)) proc))

    (define (bind proc . args)
      (apply proc args))

    ;; set!-values cannot use set! in syntax-rules templates due to
    ;; hygiene limitations.  Use let-values + begin workaround.
    (define-syntax set!-values
      (syntax-rules ()
        ((_ () expr) (begin expr (if #f #f)))
        ((_ (v1) expr)
         (let-values (((t1) expr))
           (%set!-1 v1 t1)))
        ((_ (v1 v2) expr)
         (let-values (((t1 t2) expr))
           (%set!-1 v1 t1)
           (%set!-1 v2 t2)))
        ((_ (v1 v2 v3) expr)
         (let-values (((t1 t2 t3) expr))
           (%set!-1 v1 t1)
           (%set!-1 v2 t2)
           (%set!-1 v3 t3)))))
    (define-syntax %set!-1
      (syntax-rules ()
        ((_ var val) (set! var val)))))))
|});
]

let lookup name =
  List.assoc_opt name (List.map (fun (n, s) -> (n, s)) sources)

let bundled_features =
  (* Include SRFIs registered as built-in libraries with OCaml primitives *)
  "srfi-151" :: "srfi-69" :: "srfi-14" :: "srfi-13" :: "srfi-115" ::
  List.filter_map (fun (name, _) ->
    match name with
    | ["srfi"; n] -> Some ("srfi-" ^ n)
    | _ -> None
  ) sources
