(define-structure (minisat-solver ptr))
(define-structure (minisat-var id))
(define-structure (minisat-lit id))

(define (check-minisat-solver s proc)
  (unless (minisat-solver? s)
    (error proc "not a solver" s)))

(define (check-minisat-var v proc)
  (unless (minisat-var? v)
    (error proc "not a var" v)))

(define (check-minisat-lit v proc)
  (unless (minisat-lit? v)
    (error proc "not a lit" v)))

(define (sat/new)
  (make-minisat-solver (minisat_new)))

;; TODO: can we integrate with garbage collection so that we don't
;;   have to manually delete?
(define (sat/delete s)
  (check-minisat-solver s 'sat/delete)
  (minisat_delete (minisat-solver-ptr s)))

(define (sat/new-var s)
  (check-minisat-solver s 'sat/new-var)
  (make-minisat-var (minisat_newLit (minisat-solver-ptr s))))

(define (sat/pos v)
  (check-minisat-var v 'sat/pos)
  (make-minisat-lit (minisat-var-id v)))
(define (sat/neg v)
  (check-minisat-var v 'sat/neg)
  (make-minisat-lit (minisat_negate (minisat-var-id v))))

(define (sat/add-clause! s ls)
  (check-minisat-solver s 'sat/add-clause!)
  (let ((sp (minisat-solver-ptr s)))
    (minisat_addClause_begin sp)
    (for-each
     (lambda (l)
       (check-minisat-lit l 'sat/add-clause!)
       (minisat_addClause_addLit sp (minisat-lit-id l)))
     ls)
    (minisat_addClause_commit sp)
    (void)))

(define (sat/check-sat-assuming s ls)
  (check-minisat-solver s 'sat/check-sat-assuming)
  (let ((sp (minisat-solver-ptr s)))
    (minisat_solve_begin sp)
    (for-each
     (lambda (l)
       (check-minisat-lit l 'sat/check-sat-assuming)
       (minisat_solve_addLit sp (minisat-lit-id l)))
     ls)
    (minisat_solve_commit sp)))

(define (sat/get-decisions s)
  (check-minisat-solver s 'sat/get-decisions)
  (minisat_num_decisions (minisat-solver-ptr s)))

(define (sat/get-conflicts s)
  (check-minisat-solver s 'sat/get-conflicts)
  (minisat_num_conflicts (minisat-solver-ptr s)))

(define (sat/get-propagations s)
  (check-minisat-solver s 'sat/get-propagations)
  (minisat_num_propagations (minisat-solver-ptr s)))
