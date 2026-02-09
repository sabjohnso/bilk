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
