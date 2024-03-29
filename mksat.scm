(define debug-constraints (make-parameter #f))

(define sat-solver #f)

(define (sat/hard-reset!)
  (void))

(define (sat/soft-reset!)
  (when sat-solver
    (sat/delete sat-solver))
  (set! sat-solver (sat/new)))

(define (fresh-assumption-id!)
  (set! assumption-count (+ 1 assumption-count))
  (sat/new-var sat-solver))

;; (assert (= v1 (or v2 v3)))
;; =>
;; (~v1 | v2 | v3) & (~v2 | v1) & (~v3 | v1)

;; (assert (= v1 (and v2 v3)))
;; =>
;; (~v1 | v2) & (~v1 | v3) & (~v2 | ~v3 | v1)

;; (not (and v1 v2 v3 ...))
;; =>
;; (or (not v1) (not v2) (not v3) ...)

(define (sat/constraint! type v1 v2 v3)
  (when (debug-constraints)
    (printf "sat/constraint! type: ~a v1: ~a v2: ~a v3: ~a\n" type v1 v2 v3))
  (cond
   ((eq? 'or type)
    (sat/add-clause! sat-solver (list (sat/neg v1) (sat/pos v2) (sat/pos v3)))
    (sat/add-clause! sat-solver (list (sat/neg v2) (sat/pos v1)))
    (sat/add-clause! sat-solver (list (sat/neg v3) (sat/pos v1)))
    )
   ((eq? 'and type)
    (sat/add-clause! sat-solver (list (sat/neg v1) (sat/pos v2)))
    (sat/add-clause! sat-solver (list (sat/neg v1) (sat/pos v3)))
    (sat/add-clause! sat-solver (list (sat/neg v2) (sat/neg v3) (sat/pos v1)))
    )
   (else (error 'sat/constraint "unknown type" type))))

(define (check-sat-assuming vars)
  (when (debug-constraints)
    (printf "check-sat-assuming ~a\n" vars))
  (let ([res (sat/check-sat-assuming sat-solver (map sat/pos vars))])
    (if res
      (set! unknown-count (+ 1 unknown-count))
      (set! unsat-count (+ 1 unsat-count)))
    res))

(define (sat/not-all prov)
  (when (debug-constraints)
    (printf "sat/not-all ~a\n" prov))
  (sat/add-clause! sat-solver (map sat/neg prov)))

(define (sat/log-stats!)
  (printf "decisions: ~a\n" (sat/get-decisions sat-solver))
  (printf "conflicts: ~a\n" (sat/get-conflicts sat-solver))
  (printf "propagations: ~a\n" (sat/get-propagations sat-solver))
  )
(define (sat/get-conflict)
  (let ((len (minisat_conflict_len (minisat-solver-ptr sat-solver))))
    (let loop ((acc '())
               (i (- len 1)))
      (if (< i 0)
        acc
        (loop (cons (minisat_conflict_nthLit (minisat-solver-ptr sat-solver) i) acc)
              (- i 1))))))
