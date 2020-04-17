#lang racket/base

(provide run run*
         ==
         fresh
         conde

         succeed
         fail

         use-set-var-val!-optimization
         log-stats
         check-every
         should-check-p
         debug-soundness

         hard-reset!

         assert
         scheme-eval)

(require racket/list
         racket/include
         racket/system
         (only-in rnrs/base-6 assert)
         (only-in mzlib/compat define-structure))

(define empty-intmap (hasheq))
(define (intmap-count m) (hash-count m))
(define (intmap-ref m k) (hash-ref m k (lambda () #f)))
(define (intmap-set m k v) (hash-set m k v))

;; (-> cmd (values stdout-port stdin-port stderr-port pid))
(define (process/text-ports cmd)
  (define res (process cmd))
  (values (second res) (first res) (fourth res) (second res)))

(define flush-output-port flush-output)

(define (scheme-eval c)
  (eval c))

(define make-eq-hashtable make-hasheq)
(define hashtable-ref hash-ref)
(define hashtable-set! hash-set!)
(define hashtable-contains? hash-has-key?)

(define (remp f l) (filter-not f l))
(define (exists f l) (ormap f l))
(define (list-sort f l) (sort l f))
(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))
(define (find f l)
  (cond [(memf f l) => car] [else #f]))
(define for-all andmap)




;(include "smt.scm")

(include "minisat-rkt.scm")
(include "minisat.scm")
(include "mksat.scm")

(include "mk.scm")
