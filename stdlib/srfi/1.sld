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
