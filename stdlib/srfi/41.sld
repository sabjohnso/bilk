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
