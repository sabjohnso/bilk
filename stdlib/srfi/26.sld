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
