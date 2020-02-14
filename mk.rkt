#lang racket/base

(provide run run*
         ==
         fresh
         conde
         conj2 disj2

         assert
         scheme-eval)

(require racket/list
         racket/include
         racket/system
         (only-in rnrs/base-6 assert))

(define empty-intmap (hash))
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

(include "mk.scm")
