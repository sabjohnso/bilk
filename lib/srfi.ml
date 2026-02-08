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
]

let lookup name =
  List.assoc_opt name (List.map (fun (n, s) -> (n, s)) sources)

let bundled_features =
  (* Include srfi-151 and srfi-69 which are registered as built-in libraries *)
  "srfi-151" :: "srfi-69" ::
  List.filter_map (fun (name, _) ->
    match name with
    | ["srfi"; n] -> Some ("srfi-" ^ n)
    | _ -> None
  ) sources
