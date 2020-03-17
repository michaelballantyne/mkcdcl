(library
  (mk-chez)
  (export run run*
          ==
          fresh conde

          use-set-var-val!-optimization
          ;;smt-timeout
          ;;smt-log-stmts
          log-stats
          check-every
          should-check-p
          hard-reset!

          scheme-eval

          fail
          succeed)

  (import (except (rename (chezscheme) [fold-left foldl]) subst))

  ; Trie implementation, due to Abdulaziz Ghuloum. Used for substitution
  ; and constraint store.

  ;;; subst ::= (empty)
  ;;;         | (node even odd)
  ;;;         | (data idx val)

  (define-record-type node (fields e o))

  (define-record-type data (fields idx val))

  (define shift (lambda (n) (fxsra n 1)))

  (define unshift (lambda (n i) (fx+ (fxsll n 1) i)))

  ;;; interface

  (define t:size
    (lambda (x) (t:aux:size x)))

  (define t:bind
    (lambda (xi v s)
      (unless (and (fixnum? xi) (>= xi 0))
        (error 't:bind "index must be a fixnum, got ~s" xi))
      (t:aux:bind xi v s)))

  (define t:unbind
    (lambda (xi s)
      (unless (and (fixnum? xi) (>= xi 0))
        (error 't:unbind "index must be a fixnum, got ~s" xi))
      (t:aux:unbind xi s)))

  (define t:lookup
    (lambda (xi s)
      (unless (and (fixnum? xi) (>= xi 0))
        (error 't:lookup "index must be a fixnum, got ~s" xi))
      (t:aux:lookup xi s)))

  (define t:binding-value
    (lambda (s)
      (unless (data? s)
        (error 't:binding-value "not a binding ~s" s))
      (data-val s)))

  ;;; helpers

  (define t:aux:push
    (lambda (xi vi xj vj)
      (if (fxeven? xi)
          (if (fxeven? xj)
              (make-node (t:aux:push (shift xi) vi (shift xj) vj) '())
              (make-node (make-data (shift xi) vi) (make-data (shift xj) vj)))
          (if (fxeven? xj)
              (make-node (make-data (shift xj) vj) (make-data (shift xi) vi))
              (make-node '() (t:aux:push (shift xi) vi (shift xj) vj))))))

  (define t:aux:bind
    (lambda (xi vi s*)
      (cond
        [(node? s*)
         (if (fxeven? xi)
             (make-node (t:aux:bind (shift xi) vi (node-e s*)) (node-o s*))
             (make-node (node-e s*) (t:aux:bind (shift xi) vi (node-o s*))))]
        [(data? s*)
         (let ([xj (data-idx s*)] [vj (data-val s*)])
           (if (fx= xi xj)
               (make-data xi vi)
               (t:aux:push xi vi xj vj)))]
        [else (make-data xi vi)])))

  (define t:aux:lookup
    (lambda (xi s*)
      (cond
        [(node? s*)
         (if (fxeven? xi)
             (t:aux:lookup (shift xi) (node-e s*))
             (t:aux:lookup (shift xi) (node-o s*)))]
        [(data? s*)
         (if (fx= (data-idx s*) xi)
             s*
             #f)]
        [else #f])))

  (define t:aux:size
    (lambda (s*)
      (cond
        [(node? s*) (fx+ (t:aux:size (node-e s*)) (t:aux:size (node-o s*)))]
        [(data? s*) 1]
        [else 0])))

  (define t:aux:cons^
    (lambda (e o)
      (cond
        [(or (node? e) (node? o)) (make-node e o)]
        [(data? e)
         (make-data (unshift (data-idx e) 0) (data-val e))]
        [(data? o)
         (make-data (unshift (data-idx o) 1) (data-val o))]
        [else '()])))

  (define t:aux:unbind
    (lambda (xi s*)
      (cond
        [(node? s*)
         (if (fxeven? xi)
             (t:aux:cons^ (t:aux:unbind (shift xi) (node-e s*)) (node-o s*))
             (t:aux:cons^ (node-e s*) (t:aux:unbind (shift xi) (node-o s*))))]
        [(and (data? s*) (fx= (data-idx s*) xi)) '()]
        [else s*])))


  (define empty-intmap '())
  (define (intmap-count m) (t:size m))
  (define (intmap-ref m k)
    (let ([res (t:lookup k m)])
      (if res
          (data-val res)
          #f)))
  (define (intmap-set m k v) (t:bind k v m))

  ; Misc. missing functions

  (define (remove-duplicates l)
    (cond ((null? l)
           '())
      ((member (car l) (cdr l))
       (remove-duplicates (cdr l)))
      (else
        (cons (car l) (remove-duplicates (cdr l))))))

  (define (process/text-ports cmd)
    (open-process-ports cmd 'block (native-transcoder)))

  (define scheme-eval
    (lambda (c)
      (eval c (environment '(rnrs)))))

  ;(include "smt.scm")

  (include "minisat-chez.scm")
  (include "minisat.scm")
  (include "mksat.scm")

  (include "mk.scm"))
