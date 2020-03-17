(load "test-check.scm")
(load "minisat-chez.scm")
(load "minisat.scm")

(define (do-test1)
  (define s (sat/new))

  (define x1 (sat/new-var s))
  (define x2 (sat/new-var s))

  (sat/add-clause! s (list (sat/pos x1)))
  (sat/add-clause! s (list (sat/neg x1)))

  (let ([result (sat/check-sat-assuming s '())])
    (sat/delete s)
    result))

(test "test1"
      (do-test1)
      #f)

(define (do-test2)
  (define s (sat/new))

  (define x1 (sat/new-var s))
  (define x2 (sat/new-var s))

  (sat/add-clause! s (list (sat/neg x1) (sat/pos x2)))
  (sat/add-clause! s (list (sat/pos x1)))

  (let ([result (sat/check-sat-assuming s '())])
    (sat/delete s)
    result))

(test "test2"
      (do-test2)
      #t)

(define (do-test3)
  (define s (sat/new))

  (define x1 (sat/new-var s))
  (define x2 (sat/new-var s))
  (define x3 (sat/new-var s))

  (sat/add-clause! s (list (sat/neg x1) (sat/pos x3)))
  (sat/add-clause! s (list (sat/neg x1) (sat/pos x2)))
  (sat/add-clause! s (list (sat/neg x3) (sat/neg x2) (sat/pos x1)))

  (let ([result (sat/check-sat-assuming s (list (sat/pos x1)))])
    (sat/delete s)
    result))

(test "test3"
      (do-test3)
      #t)

(define (test3-low)
  (define s (minisat_new))

  (define x1 (minisat_newLit s))
  (define x2 (minisat_newLit s))
  (define x3 (minisat_newLit s))

  (minisat_addClause_begin s)
  (minisat_addClause_addLit s (minisat_negate x1))
  (minisat_addClause_addLit s x3)
  (minisat_addClause_commit s)

  (minisat_addClause_begin s)
  (minisat_addClause_addLit s (minisat_negate x1))
  (minisat_addClause_addLit s x2)
  (minisat_addClause_commit s)

  (minisat_addClause_begin s)
  (minisat_addClause_addLit s (minisat_negate x3))
  (minisat_addClause_addLit s (minisat_negate x2))
  (minisat_addClause_addLit s x1)
  (minisat_addClause_commit s)

  (minisat_solve_begin s)
  (minisat_solve_addLit s x1)
  (let ([result (minisat_solve_commit s)])
    (minisat_delete s)
    result))

(test "test3, low level"
      (test3-low)
      #t)

