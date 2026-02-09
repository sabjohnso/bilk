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
