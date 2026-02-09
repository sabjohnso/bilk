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
