(load-shared-object "../minisat-c-bindings/install/lib/libminisat-c.so.1")

(define minisat_new (foreign-procedure "minisat_new" () uptr))

(define minisat_newLit (foreign-procedure "minisat_newLit" (uptr) int))
(define minisat_negate (foreign-procedure "minisat_negate" (int) int))

(define minisat_addClause_begin (foreign-procedure "minisat_addClause_begin" (uptr) void))
(define minisat_addClause_addLit (foreign-procedure "minisat_addClause_addLit" (uptr int) void))
(define minisat_addClause_commit (foreign-procedure "minisat_addClause_commit" (uptr) boolean))

(define minisat_solve_begin (foreign-procedure "minisat_solve_begin" (uptr) void))
(define minisat_solve_addLit (foreign-procedure "minisat_solve_addLit" (uptr int) void))
(define minisat_solve_commit (foreign-procedure "minisat_solve_commit" (uptr) boolean))

(define minisat_okay (foreign-procedure "minisat_okay" (uptr) boolean))

(define minisat_delete (foreign-procedure "minisat_delete" (uptr) void))
