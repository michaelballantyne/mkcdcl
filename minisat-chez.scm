(load-shared-object "../minisat-c-bindings/install/lib/libminisat-c.so.1")

(define minisat_new (foreign-procedure "minisat_new" () uptr))

(define minisat_delete (foreign-procedure "minisat_delete" (uptr) void))

  
