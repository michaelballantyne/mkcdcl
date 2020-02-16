#lang racket/base

(provide run run*
         ==
         fresh
         conde

         use-set-var-val!-optimization
         smt-timeout
         smt-log-unknowns
         smt-log-stmts
         smt-log-sat-ratio
         smt-should-check-every
         smt-should-check-p

         assert
         scheme-eval)

(require racket/list
         racket/include
         racket/system
         (only-in rnrs/base-6 assert))

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


(include "mk.scm")
