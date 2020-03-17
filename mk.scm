(define use-set-var-val!-optimization (make-parameter #t))
(define log-stats (make-parameter #f))
(define check-every (make-parameter 15))
(define should-check-p
  (make-parameter
    (lambda (cnt)
      (let ([v (check-every)])
        (and v cnt (>= cnt v))))))

(define new-scope
  (lambda ()
    (list 'scope)))

(define nonlocal-scope
  (list 'non-local-scope))

(define scope-eq? eq?)

(define unbound (list 'unbound))

(define var
  (let ((counter -1))
    (lambda (scope)
      (set! counter (+ 1 counter))
      (vector unbound scope counter))))

(define (var? x)
  (vector? x))

(define var-val
  (lambda (x)
    (vector-ref x 0)))

(define set-var-val!
  (lambda (x v)
    (vector-set! x 0 v)))

(define var-scope
  (lambda (x)
    (vector-ref x 1)))

(define var-idx
  (lambda (x)
    (vector-ref x 2)))

(define initial-scope (new-scope))
(define empty-subst-map empty-intmap)
(define subst-map-length intmap-count)
(define (subst-map-lookup u S)
  (intmap-ref S (var-idx u)))
(define (subst-map-add S var val)
  (intmap-set S (var-idx var) val))

(define subst
  (lambda (mapping scope)
    (cons mapping scope)))

(define subst-map car)

(define subst-scope cdr)

(define subst-length
  (lambda (S)
    (subst-map-length (subst-map S))))

(define subst-with-scope
  (lambda (S new-scope)
    (subst (subst-map S) new-scope)))

(define empty-subst (subst empty-subst-map initial-scope))

(define subst-add
  (lambda (S x v)
    ; set-var-val! optimization: set the value directly on the
    ; variable object if we haven't branched since its creation
    ; (the scope of the variable and the substitution are the same).
    ; Otherwise extend the substitution mapping.
    (if (and (use-set-var-val!-optimization) (scope-eq? (var-scope x) (subst-scope S)))
      (begin
        (set-var-val! x v)
        S)
      (subst (subst-map-add (subst-map S) x v) (subst-scope S)))))

(define subst-lookup
  (lambda (u S)
    ; set-var-val! optimization.
    ; Tried checking the scope here to avoid a subst-map-lookup
    ; if it was definitely unbound, but that was slower.
    (if (not (eq? (var-val u) unbound))
      (var-val u)
      (subst-map-lookup u (subst-map S)))))


(define unification-count 0)

(define sat-count 0)
(define unsat-count 0)
(define unknown-count 0)
(define (reset-sat-counts!)
  (set! sat-count 0)
  (set! unsat-count 0)
  (set! unknown-count 0))

;; State: (list/c Counter Substitution AssertionHistory)
(define (state c s ah) (list c s ah))
(define state-counter car)
(define state-s cadr)
(define state-assertion-history caddr)
(define (state-s-set st s)
  (state (state-counter st) s (state-assertion-history st)))
(define (extend-assertion-history st ctx)
  (state (state-counter st) (state-s st)
         (cons (ctx->assertion-var ctx) (state-assertion-history st))))
(define state-with-scope
  (lambda (st new-scope)
    (state (state-counter st)
           (subst-with-scope (state-s st) new-scope)
           (state-assertion-history st))))

;; AssertionHistory: (listof AssumptionVariableId)
;; Substitution: (intmap/c Variable (cons/c Term ProvenanceSet))
;; ProvenanceSet: (listof AssumptionVariableId)
(define empty-assertion-history '())
(define empty-state `(0 ,empty-subst ,empty-assertion-history))

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
	    (sat/constraint type v1 v2 v3))
          (values l r)))))

;; Counter: Integer (used to decide whether to actually call the solver)
(define (inc-counter st)
  (if (car st)
    (cons (+ 1 (car st)) (cdr st))
    st))

(define (reset-counter st)
  (if (car st)
    (cons 0 (cdr st))
    st))
(define get-counter car)

(define (fail-counter st)
  (cons #f (cdr st)))

(define (update-stats! final)
  (let ([total (+ sat-count unsat-count unknown-count)])
    (when (or final (> total 500))
      (when (log-stats)
        (printf "sat count: ~a\n" sat-count)
        (printf "unsat count: ~a\n" unsat-count)
        (printf "unknown count: ~a\n" unknown-count)

        (printf "total unifications: ~a\n" unification-count)
        ;;(printf "total assumption variables: ~a\n" assumption-count)
        (printf "\n")
        )
      (reset-sat-counts!))))

(define check-sometimes
  (lambda (st)
    (if ((should-check-p) (get-counter st))
        (begin
          (update-stats! #f)
          (check (reset-counter st))
          #;(if (check (reset-counter st))
            st
            (fail-counter st)))
        (inc-counter st))))

(define check
  (lambda (st)
    (let ((vars (state-assertion-history st)))
    (if (check-sat-assuming vars)
      st
      #f))))


(define (cdcl/conflict prov st)
  ;; OK to be ephemeral, only boost
  (sat/not-all prov)
  #f)

(define purge
  (lambda (ctx)
    (lambda (st) (update-stats! #t) st)))


;;(define (provenance-union . args) (apply append args))
(define provenance-union append)
(define empty-provenance '())

;; walk: Term, Substitution -> Term, ProvenanceSet
(define walk
  (lambda (v s)
    (cond
      ((var? v)
       (let ((a (subst-lookup v s)))
         (cond
           (a (let-values (((t prov) (walk (car a) s)))
                          (values t (provenance-union (cdr a) prov))))
           (else (values v empty-provenance)))))
      (else (values v empty-provenance)))))

(define ext-s
  (lambda (x v s prov)
    (subst-add s x `(,v . ,prov))))


;; Example 1
;;
;; 1. (== x y)
;; 2. (== x z)
;;
;;  ((x y (1))
;;   (y z (2 1)))  This needs 1 as well so that we include constraint 1 in the provenance set for the failure later at 4
;;
;; 3. (== z 1)
;;
;;  (z 1 2)
;;
;; 4. (== y 2)
;; Final blame: (1 2 3 4)

;; Term,Term,Substitution,Provenance -> (#t, Substitution) or (#f, Provenance)
(define unify-check
  (lambda (u v s new-prov)
    (let-values (((u u-prov) (walk u s))
                 ((v v-prov) (walk v s)))
      (let ((p (provenance-union new-prov u-prov v-prov))) ;; TODO
        (cond
          ((eq? u v) (values #t s))
          ((var? u) (ext-s-check u v s p))
          ((var? v) (ext-s-check v u s p))
          ((and (pair? u) (pair? v))
           (let-values (((success? s)
                         (unify-check 
                          (car u) (car v) s p)))
             (if success?
                 (unify-check 
                  (cdr u) (cdr v) s p)
                 (values #f s))))
          ((equal? u v) (values #t s))
          (else (values #f p)))))))

(define ext-s-check
  (lambda (x v s prov)
    (cond
      ((occurs-check x v s) (values #f prov))
      (else (values #t (ext-s x v s prov))))))

(define occurs-check
  (lambda (x v s)
    (let-values (((v _) (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) 
         (or 
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))

(define walk*
  (lambda (w s)
    (let-values (((v _) (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let-values (((v _) (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (subst-length s)) s '()))
        ((pair? v) (reify-s (cdr v)
                     (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v)
    (lambda (st)
      (let ([st (state-with-scope st nonlocal-scope)])
        (let ((s (state-s st)))
          (let ((v (walk* v s)))
            (walk* v (reify-s v empty-subst))))))))



(define (prov-from-ctx ctx) (list (ctx->assertion-var ctx)))
(define (== u v)
  (lambda (ctx)
    (lambda (st)
      (set! unification-count (+ 1 unification-count))
      (let-values
          (((success? s)
            (unify-check u v (state-s st) (prov-from-ctx ctx))))
        (if success?
            (extend-assertion-history (state-s-set st s) ctx)
            (cdcl/conflict s st))))))

;Search

; SearchStream: #f | Procedure | State | (Pair State (-> SearchStream))

; SearchStream constructor types. Names inspired by the plus monad?

; -> SearchStream
(define mzero (lambda () #f))

; c: State
; -> SearchStream
(define unit (lambda (c) c))

; c: State
; f: (-> SearchStream)
; -> SearchStream
;
; f is a thunk to avoid unnecessary computation in the case that c is
; the last answer needed to satisfy the query.
(define choice (lambda (c f) (cons c f)))

; e: SearchStream
; -> (-> SearchStream)
(define-syntax inc
  (syntax-rules ()
    ((_ e) (lambda () e))))

; Goal: (State -> SearchStream)

; e: SearchStream
; -> Goal
(define-syntax lambdag@
  (syntax-rules ()
    ((_ (st) e) (lambda (st) e))))

; Match on search streams. The state type must not be a pair with a
; procedure in its cdr.
;
; (() e0)     failure
; ((f) e1)    inc for interleaving. separate from success or failure
;               to ensure it goes all the way to the top of the tree.
; ((c) e2)    single result. Used rather than (choice c (inc (mzero)))
;               to avoid returning to search a part of the tree that
;               will inevitably fail.
; ((c f) e3)  multiple results.
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ((c-inf e))
       (cond
         ((not c-inf) e0)
         ((procedure? c-inf)  (let ((f^ c-inf)) e1))
         ((not (and (pair? c-inf)
                 (procedure? (cdr c-inf))))
          (let ((c^ c-inf)) e2))
         (else (let ((c (car c-inf)) (f (cdr c-inf)))
                 e3)))))))

; c-inf: SearchStream
;     f: (-> SearchStream)
; -> SearchStream
;
; f is a thunk to avoid unnecesarry computation in the case that the
; first answer produced by c-inf is enough to satisfy the query.
(define mplus
  (lambda (c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((c) (choice c f))
      ((c f^) (choice c (inc (mplus (f) f^)))))))

; c-inf: SearchStream
;     g: Goal
; -> SearchStream
(define bind
  (lambda (c-inf g)
    (case-inf c-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((c) (g c))
      ((c f) (mplus (g c) (inc (bind (f) g)))))))

; Int, SearchStream -> (ListOf SearchResult)
(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (take n f))
         ((c) (cons c '()))
         ((c f) (cons c
                  (take (and n (- n 1)) f))))))))

; -> Goal
(define (conj2 ig1 ig2)
     (lambda (ctx)
       (let-values (((ctx1 ctx2) (get-child-assumptions+assert! ctx 'and)))
         (let ([g1 (ig1 ctx1)] [g2 (ig2 ctx2)])
           (lambdag@ (st)
                     (let ([st (check-sometimes (extend-assertion-history st ctx))])
                       (and st (bind (g1 st) g2))))))))

(define-syntax conj*
  (syntax-rules ()
    ((_ ig) ig)
    ((_ ig0 ig1 ig ...) (conj* (conj2 ig0 ig1) ig ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) ig0 ig ...)
     (lambda (ctx)
       (lambdag@ (st)
                 (inc
                   (let ([scope (subst-scope (state-s st))])
                     (let ((x (var scope)) ...)
                       (((conj* ig0 ig ...) ctx) st)))))))))

; -> Goal
(define (disj2 ig1 ig2)
  (lambda (ctx)
    (let-values (((ctx1 ctx2) (get-child-assumptions+assert! ctx 'or)))
      (let ([g1 (ig1 ctx1)] [g2 (ig2 ctx2)])
        (lambdag@ (st)
                  (let ([st (check-sometimes (extend-assertion-history st ctx))])
                    (and st (mplus (g1 st) (inc (g2 st))))))))))

(define-syntax disj*
  (syntax-rules ()
    ((_ ig) ig)
    ((_ ig0 ig ...) (disj2 ig0 (disj* ig ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (ig0 ig ...) (ig1 ig^ ...) ...)
     (lambda (ctx)
       (lambdag@ (st)
         (inc
           (let ([st (state-with-scope st (new-scope))])
             (((disj*
                 (conj* ig0 ig ...)
                 (conj* ig1 ig^ ...) ...)
               ctx)
              st))))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) ig ...)
     (begin
       (soft-reset!)
       (let ((ctx (initial-ctx)))
         ;;TODO: keep? (smt-call (list `(assert ,(ctx->assertion-var ctx))))
         (let ((q (var initial-scope)))
           (map (reify q)
                (take n
                      (inc
                       (((conj* ig ... purge) ctx)
                        empty-state))))))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

(define fail (lambda (ctx) (lambda (st) #f)))
(define succeed (lambda (ctx) (lambda (st) st)))

(hard-reset!)
