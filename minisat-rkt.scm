(require ffi/unsafe ffi/unsafe/define)

(define-ffi-definer define-minisat
  (ffi-lib "minisat-c-bindings/install/lib/libminisat-c.so.1"))

(define-minisat minisat_new (_fun -> _pointer))

(define-minisat minisat_newLit (_fun _pointer -> _int))
(define-minisat minisat_negate (_fun _int -> _int))

(define-minisat minisat_addClause_begin (_fun _pointer -> _void))
(define-minisat minisat_addClause_addLit (_fun _pointer _int -> _void))
(define-minisat minisat_addClause_commit (_fun _pointer -> _bool))

(define-minisat minisat_solve_begin (_fun _pointer -> _void))
(define-minisat minisat_solve_addLit (_fun _pointer _int -> _void))
(define-minisat minisat_solve_commit (_fun _pointer -> _bool))

(define-minisat minisat_okay (_fun _pointer -> _bool))

(define-minisat minisat_delete (_fun _pointer -> _void))

(define-minisat minisat_num_decisions (_fun _pointer -> _int))
