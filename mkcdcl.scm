(define log-stats (make-parameter #f))
(define check-every (make-parameter 15))
(define should-check-p
  (make-parameter
    (lambda (cnt)
      (let ([v (check-every)])
        (and v (number? cnt) (>= cnt v))))))
(define debug-soundness (make-parameter #f))

(define assoc-term car)
(define assoc-prov cdr)

;; AssertionHistory: (listof AssumptionVariableId)
(define empty-assertion-history '())

(define (extend-assertion-history st ctx)
  (state-with-assertion-history
    st
    (cons (ctx->assertion-var ctx) (state-assertion-history st))))

; Counter: (or/c #f integer?)
;   Used to decide whether to actually call the solver.
;   #f is used for debug-soundness to record a soundness failure;
;     at that point no further CDCL checks are performed for that state.
(define (inc-counter st)
  (if (number? (state-counter st))
    (state-with-counter st (+ 1 (state-counter st)))
    st))

(define (reset-counter st)
  (if (number? (state-counter st))
    (state-with-counter st 0)
    st))

(define (fail-counter st)
  (state-with-counter st (state-assertion-history st)))

; Context: (cons/c AssumptionID
;                  (box/c
;                    (or/c #f
;                          (cons/c Context Context))))
;
; Represents a goal's position in the static AND / OR tree.
;
; Contains:
;    An AssumptionID representing the current subtree's truth value in the solver
;    A box that may contain child contexts. When the first search thread unfolds children,
;     it assigns them child contexts and assumption IDs.

(define (empty-ctx) (cons (fresh-assumption-id!) (box #f)))
(define (initial-ctx) (empty-ctx))
(define (ctx->assertion-var ctx)
  (car ctx))
(define (get-child-assumptions+assert! ctx type)
  (let ((r (unbox (cdr ctx))))
    (if r
      (values (car r) (cdr r))
      (let ([l (empty-ctx)] [r (empty-ctx)])
        (set-box! (cdr ctx) (cons l r))
        (let ((v1 (ctx->assertion-var ctx))
              (v2 (ctx->assertion-var l))
              (v3 (ctx->assertion-var r)))
          (sat/constraint! type v1 v2 v3))
        (values l r)))))

; Provenance: (listof AssumptionVariableId)
(define empty-provenance '())
(define (prov-from-ctx ctx) (list (ctx->assertion-var ctx)))
(define provenance-union lset-union-equal?)
(define (provenance-length p) (length p))
(define (provenance->list p) p)

; Statistics counters
(define unification-count 0)
(define assumption-count 0)
(define cutoff-counts (make-eq-hashtable))

(define sat-count 0)
(define unsat-count 0)
(define unknown-count 0)
(define (reset-sat-counts!)
  (set! sat-count 0)
  (set! unsat-count 0)
  (set! unknown-count 0))

; Prints stats every 1000 calls, or always when final is true. Resets SAT counts to 0.
(define (update-stats! final)
  (let ([total (+ sat-count unsat-count unknown-count)])
    (when (or final (> total 1000))
      (when (log-stats)
        (printf "sat count: ~a\n" sat-count)
        (printf "unsat count: ~a\n" unsat-count)
        (printf "unknown count: ~a\n" unknown-count)
        (printf "total unifications: ~a\n" unification-count)
        (printf "total assumption variables: ~a\n" assumption-count)
        (sat/log-stats!)
        (printf "\n"))
      (reset-sat-counts!))))

(define (cdcl/conflict prov st)
  ;; OK to be ephemeral, only boost
  (sat/not-all (provenance->list prov))
  #f)

(define check
  (lambda (st)
    (let ((vars (state-assertion-history st)))
    (if (check-sat-assuming vars)
      st
      #f))))

(define check-sometimes
  (lambda (st)
    (when (not (number? (state-counter st)))
      (hashtable-set! cutoff-counts (state-counter st)
                      (+ 1 (hashtable-ref cutoff-counts (state-counter st) 0))))
    (if ((should-check-p) (state-counter st))
        (begin
          (update-stats! #f)
          (let ([checked (check (reset-counter st))])
            (if (debug-soundness)
              (or checked (fail-counter st))
              checked)))
        (inc-counter st))))

(define purge
  (lambda (ctx)
    (lambda (st)
      (when (and (debug-soundness) (not (number? (state-counter st))))
        (error 'purge "CDCL soundness bug" st))
      (update-stats! #t) st)))

(define (hard-reset!)
  (sat/hard-reset!)

  (soft-reset!))

(define (soft-reset!)
  (set! unification-count 0)
  (set! assumption-count 0)
  (reset-sat-counts!)

  (sat/soft-reset!))

(hard-reset!)
