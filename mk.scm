(define use-set-var-val!-optimization (make-parameter #t))
(define log-stats (make-parameter #f))
(define check-every (make-parameter 15))
(define should-check-p
  (make-parameter
    (lambda (cnt)
      (let ([v (check-every)])
        (and v cnt (>= cnt v))))))
(define debug-soundness (make-parameter #f))


; Scope object.
; Used to determine whether a branch has occured between variable
; creation and unification to allow the set-var-val! optimization
; in subst-add. Both variables and substitutions will contain a
; scope. When a substitution flows through a conde it is assigned
; a new scope.

; Creates a new scope that is not scope-eq? to any other scope
(define new-scope
  (lambda ()
    (list 'scope)))

(define nonlocal-scope
  (list 'non-local-scope))

(define scope-eq? eq?)

; Logic variable object.
; Contains:
;   val - value for variable assigned by unification using
;      set-var-val! optimization. unbound if not yet set or
;      stored in substitution.
;   scope - scope that the variable was created in.
;   idx - unique numeric index for the variable. Used by the
;      trie substitution representation.
; Variable objects are compared by object identity.

; The unique val for variables that have not yet been bound
; to a value or are bound in the substitution
(define unbound (list 'unbound))

(define var
  (let ((counter -1))
    (lambda (scope)
      (set! counter (+ 1 counter))
      (vector unbound scope counter))))

; Vectors are not allowed as terms, so terms that are vectors
; are variables.
(define (var? x)
  (vector? x))

(define var-eq? eq?)

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


; Substitution: (cons Mapping Scope)
; Mapping: (intmap-of Var (cons Term Provenance)
;
; Contains:
;   map - mapping of logic variables to terms and provenances
;   scope - scope at current program point, for set-var-val!
;     optimization. Updated at conde. Included in the substitution
;     because it is required to fully define the substitution
;     and how it is to be extended.
;
; The map is implemented with an intmap, whose implementation depends on the Scheme used.
; mk-vicare.scm.

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
  (lambda (S x v p)
    (let ([rhs (cons v p)])
      ; set-var-val! optimization: set the value directly on the
      ; variable object if we haven't branched since its creation
      ; (the scope of the variable and the substitution are the same).
      ; Otherwise extend the substitution mapping.
      (if (and (use-set-var-val!-optimization) (scope-eq? (var-scope x) (subst-scope S)))
        (begin
          (set-var-val! x rhs)
          S)
        (subst (subst-map-add (subst-map S) x rhs) (subst-scope S))))))

(define subst-lookup
  (lambda (u S)
    ; set-var-val! optimization.
    ; Tried checking the scope here to avoid a subst-map-lookup
    ; if it was definitely unbound, but that was slower.
    (if (not (eq? (var-val u) unbound))
      (var-val u)
      (subst-map-lookup u (subst-map S)))))

(define assoc-term car)
(define assoc-prov cdr)


; ConstraintRecord
;
; Describes the constraints attached to a single variable.
;
; Contains:
;   T - type constraint. 'symbolo 'numbero or #f to indicate
;         no constraint
;   D - list of disequality constraints. Each disequality is a list of
;         associations. The constraint is violated if all associated
;         variables are equal in the substitution simultaneously. D
;         could contain duplicate constraints (created by distinct =/=
;         calls). A given disequality constraint is only attached to
;         one of the variables involved, as all components of the
;         constraint must be violated to cause failure.
;   A - list of absento constraints. Each constraint is a ground atom.
;         The list contains no duplicates.

(define empty-c `(#f () ()))

(define c-T
  (lambda (c)
    (car c)))

(define c-D
  (lambda (c)
    (cadr c)))

(define c-A
  (lambda (c)
    (caddr c)))

(define c-with-T
  (lambda (c T)
    (list T (c-D c) (c-A c))))

(define c-with-D
  (lambda (c D)
    (list (c-T c) D (c-A c))))

(define c-with-A
  (lambda (c A)
    (list (c-T c) (c-D c) A)))

; ConstraintStore: (intmap-of VarID ConstraintRecord)

(define empty-cs empty-intmap)

(define set-c
  (lambda (st v c)
    (state-cs-set
      st
      (intmap-set (state-cs st) (var-idx v) c))))

(define lookup-c
  (lambda (st v)
    (let ((res (intmap-ref (state-cs st) (var-idx v))))
      (or res empty-c))))

; t:unbind in mk-chez.scm either is buggy or doesn't do what I would expect, so
; I implement remove by setting the value to the empty constraint record.
(define remove-c
  (lambda (v st)
    (state-cs-set st (intmap-set (state-cs st) (var-idx v) empty-c))))


;; AssertionHistory: (listof AssumptionVariableId)
(define empty-assertion-history '())


;; State: (list/c Counter Substitution ConstraintStore AssertionHistory)
(define (state ct s c ah) (list ct s c ah))
(define state-counter car)
(define state-s cadr)
(define state-cs caddr)
(define state-assertion-history cadddr)

(define empty-state `(0 ,empty-subst ,empty-cs ,empty-assertion-history))

(define (state-counter-set st c)
  (cons c (cdr st)))

(define (state-s-set st s)
  (state (state-counter st)
         s
         (state-cs st)
         (state-assertion-history st)))

(define (state-cs-set st c)
  (state (state-counter st)
         (state-s st)
         c
         (state-assertion-history st)))

(define (state-assertion-history-set st h)
  (state (state-counter st)
         (state-s st)
         (state-cs st)
         h))


(define (extend-assertion-history st ctx)
  (state-assertion-history-set
    st
    (cons (ctx->assertion-var ctx) (state-assertion-history st))))

(define (state-with-scope st new-scope)
  (state-s-set
    st
    (subst-with-scope (state-s st) new-scope)))


; Counter: (or/c #f integer?)
;   Used to decide whether to actually call the solver.
;   #f is used for debug-soundness to record a soundness failure;
;     at that point no further CDCL checks are performed for that state.
(define (inc-counter st)
  (if (state-counter st)
    (state-counter-set st (+ 1 (state-counter st)))
    st))

(define (reset-counter st)
  (if (state-counter st)
    (state-counter-set st 0)
    st))

(define (fail-counter st)
  (state-counter-set st #f))

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
          (sat/constraint type v1 v2 v3))
        (values l r)))))

; Provenance: (listof AssumptionVariableId)
(define empty-provenance '())
(define (prov-from-ctx ctx) (list (ctx->assertion-var ctx)))
(define provenance-union append)

; Statistics counters
(define unification-count 0)
(define assumption-count 0)

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
  (sat/not-all prov)
  #f)

(define check
  (lambda (st)
    (let ((vars (state-assertion-history st)))
    (if (check-sat-assuming vars)
      st
      #f))))

(define check-sometimes
  (lambda (st)
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
      (when (and (debug-soundness) (not (state-counter st)))
        (error 'purge "CDCL soundness bug" st))
      (update-stats! #t) st)))


;;; Constraints

; walk: Term, Substitution -> Term, ProvenanceSet
(define walk
  (lambda (v s)
    (cond
      ((var? v)
       (let ((a (subst-lookup v s)))
         (cond
           (a (let-values (((t prov) (walk (assoc-term a) s)))
                          (values t (provenance-union (assoc-prov a) prov))))
           (else (values v empty-provenance)))))
      (else (values v empty-provenance)))))

; Term, Term, Substitution, Provenance -> (Substitution, Added) or (#f, Provenance)
(define unify
  (lambda (u v s new-prov)
    (let-values (((u u-prov) (walk u s))
                 ((v v-prov) (walk v s)))
      (let ((p (provenance-union new-prov u-prov v-prov))) ;; TODO
        (cond
          ((eq? u v) (values s '()))
          ((var? u) (ext-s-check u v s p))
          ((var? v) (ext-s-check v u s p))
          ((and (pair? u) (pair? v))
           (let-values (((s added-or-p-car)
                         (unify
                          (car u) (car v) s p)))
             (if s
               (let-values (((s added-or-p-cdr) (unify (cdr u) (cdr v) s p)))
                 (if s
                   (values s (append added-or-p-car added-or-p-cdr))
                   (values s added-or-p-cdr)))
               (values #f added-or-p-car))))
          ((equal? u v) (values s '()))
          (else (values #f p)))))))

;(define ext-s-check
  ;(lambda (x v S)
    ;(cond
      ;((occurs-check x v S) (values #f #f))
      ;(else (values (subst-add S x v) `((,x . ,v)))))))


(define ext-s-check
  (lambda (x v s prov)
    (let-values (((occurs? prov^) (occurs-check x v s prov)))
      (if occurs?
        (values #f prov^)
        (values (subst-add s x v prov) (list (cons x v)))))))

(define occurs-check
  (lambda (x v s prov)
    (let-values (((v v-prov) (walk v s)))
      (let ([p (provenance-union v-prov prov)])
        (cond
          ((var? v)
           (if (var-eq? v x)
             (values #t p)
             (values #f (void))))
          ((pair? v)
           (let-values (((occurs? res-prov)
                         (occurs-check x (car v) s p)))
             (if occurs?
               (values #t res-prov)
               (occurs-check x (cdr v) s p))))
          (else (values #f (void))))))))


; Constraints
; C refers to the constraint store map
; c refers to an individual constraint record

; Constraint: State -> #f | State
;
; (note that a Constraint is a Goal but a Goal is not a Constraint.
;  Constraint implementations currently use this more restrained type.
;  See `and-foldl` and `update-constraints`.)

; Requirements for type constraints:
; 1. Must be positive, not negative. not-pairo wouldn't work.
; 2. Each type must have infinitely many possible values to avoid
;      incorrectness in combination with disequality constraints,
;      like: (fresh (x) (booleano x) (=/= x #t) (=/= x #f))
(define type-constraint
  (lambda (type-pred type-id)
    (lambda (u)
      (lambda (ctx)
        (lambdag@ (st)
          (let-values (((term TODO) (walk u (state-s st))))
            (cond
              ((type-pred term) st)
              ((var? term)
               (let* ((c (lookup-c st term))
                     (T (c-T c)))
                 (cond
                   ((eq? T type-id) st)
                   ((not T) (set-c st term (c-with-T c type-id)))
                   (else #f))))
              (else #f))))))))

(define symbolo (type-constraint symbol? 'symbolo))
(define numbero (type-constraint number? 'numbero))

(define (add-to-D st v d)
  (let* ((c (lookup-c st v))
         (c^ (c-with-D c (cons d (c-D c)))))
    (set-c st v c^)))

(define =/=*
  (lambda (S+)
    (lambda (ctx)
      (lambdag@ (st)
        (let-values (((S added) (unify* S+ (subst-with-scope
                                             (state-s st)
                                             nonlocal-scope))))
          (cond
            ((not S) st)
            ((null? added) #f)
            (else
              ; Choose one of the disequality elements (el) to attach
              ; the constraint to. Only need to choose one because
              ; all must fail to cause the constraint to fail.
              (let ((el (car added)))
                (let ((st (add-to-D st (car el) added)))
                  (if (var? (cdr el))
                    (add-to-D st (cdr el) added)
                    st))))))))))

(define =/=
  (lambda (u v)
    (=/=* `((,u . ,v)))))

(define absento
  (lambda (ground-atom term)
    (unless (or (symbol? ground-atom)
                (number? ground-atom)
                (boolean? ground-atom)
                (null? ground-atom))
      (error 'absento "first argument to absento must be a ground atom"))
    (lambda (ctx)
      (lambdag@ (st)
        (let-values (((term TODO) (walk term (state-s st))))
          (cond
            ((pair? term)
             (let ((st^ (((absento ground-atom (car term)) ctx) st)))
               (and st^ (((absento ground-atom (cdr term)) ctx) st^))))
            ((eqv? term ground-atom) #f)
            ((var? term)
             (let* ((c (lookup-c st term))
                    (A (c-A c)))
               (if (memv ground-atom A)
                 st
                 (let ((c^ (c-with-A c (cons ground-atom A))))
                   (set-c st term c^)))))
            (else st)))))))

; Fold lst with proc and initial value init. If proc ever returns #f,
; return with #f immediately. Used for applying a series of
; constraints to a state, failing if any operation fails.
(define (and-foldl proc init lst)
  (if (null? lst)
    init
    (let ([res (proc (car lst) init)])
      (and res (and-foldl proc res (cdr lst))))))


; Not fully optimized. Could do absento update with fewer
; hash-refs / hash-sets.
(define update-constraints
  (lambda (ctx)
    (lambda (a st)
      (let ([old-c (lookup-c st (lhs a))])
        (if (eq? old-c empty-c)
          st
          (let ((st (remove-c (lhs a) st)))
           (and-foldl (lambda (op st) ((op ctx) st)) st
            (append
              (if (eq? (c-T old-c) 'symbolo)
                (list (symbolo (rhs a)))
                '())
              (if (eq? (c-T old-c) 'numbero)
                (list (numbero (rhs a)))
                '())
              (map (lambda (atom) (absento atom (rhs a))) (c-A old-c))
              (map (lambda (d) (=/=* d)) (c-D old-c))))))))))

(define (== u v)
  (lambda (ctx)
    (lambda (st)
      (set! unification-count (+ 1 unification-count))
      (let-values
          (((s added-or-prov)
            (unify u v (state-s st) (prov-from-ctx ctx))))
        (if s
            #;(extend-assertion-history (state-s-set st s) ctx)
            (and-foldl (update-constraints ctx) (extend-assertion-history (state-s-set st s) ctx)
                       added-or-prov)
            (cdcl/conflict added-or-prov st))))))

;;; Reification


(define lhs car)
(define rhs cdr)

(define (walk/reifier v S)
  (let-values (((res prov) (walk v S))) res))

; Version of unification without provenance but with extra return of added assocations,
; used by the faster-mk reifier for disequalities.
(define (unify/reifier u v S)
  (unify u v S empty-provenance))

(define unify*
  (lambda (S+ S)
    (unify/reifier (map lhs S+) (map rhs S+) S)))

(define walk*
  (lambda (v S)
    (let ((v (walk/reifier v S)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) S) (walk* (cdr v) S)))
        (else v)))))

(define vars
  (lambda (term acc)
    (cond
      ((var? term) (cons term acc))
      ((pair? term)
       (vars (cdr term) (vars (car term) acc)))
      (else acc))))

; Create a constraint store of the old representation from a state
; object, so that we can use the old reifier. Only accumulates
; constraints related to the variable being reified which makes things
; a bit faster.
(define c-from-st
  (lambda (st x)
    (let ((vs (vars (walk* x (state-s st)) '())))
      (foldl
        (lambda (v c-store)
          (let ((c (lookup-c st v)))
            (let ((S (state-s st))
                  (D (c->D c-store))
                  (Y (c->Y c-store))
                  (N (c->N c-store))
                  (T (c->T c-store))
                  (T^ (c-T c))
                  (D^ (c-D c))
                  (A^ (c-A c)))
              `(,S
                 ,(append D^ D)
                 ,(if (eq? T^ 'symbolo)
                    (cons v Y)
                    Y)
                 ,(if (eq? T^ 'numbero)
                    (cons v N)
                    N)
                 ,(append
                    (map (lambda (atom) (cons atom v)) A^)
                    T)))))
        `(,(state-s st) () () () ())
        (remove-duplicates vs)))))

(define reify
  (lambda (x)
    (lambda (st)
      (let ((st (state-with-scope st nonlocal-scope)))
        (let ((c (c-from-st st x)))
          (let ((c (cycle c)))
            (let* ((S (c->S c))
                   (D (walk* (c->D c) S))
                   (Y (walk* (c->Y c) S))
                   (N (walk* (c->N c) S))
                   (T (walk* (c->T c) S)))
              (let ((v (walk* x S)))
                (let ((R (reify-S v (subst empty-subst-map
                                           nonlocal-scope))))

                  (reify+ v R
                          (let ((D (remp
                                     (lambda (d)
                                       (let ((dw (walk* d S)))
                                         (anyvar? dw R)))
                                     (rem-xx-from-d c))))
                            (rem-subsumed D))
                          (remp
                            (lambda (y) (var? (walk/reifier y R)))
                            Y)
                          (remp
                            (lambda (n) (var? (walk/reifier n R)))
                            N)
                          (remp (lambda (t)
                                  (anyvar? t R)) T)))))))))))


; Bits from the old constraint implementation, still used for
; reification.

; In this part of the code, c refers to the
; old constraint store with components:
; S - substitution
; D - disequality constraints
; Y - symbolo
; N - numbero
; T - absento

(define c->S (lambda (c) (car c)))
(define c->D (lambda (c) (cadr c)))
(define c->Y (lambda (c) (caddr c)))
(define c->N (lambda (c) (cadddr c)))
(define c->T (lambda (c) (cadddr (cdr c))))

; Syntax for reification goal objects using the old constraint store
(define-syntax lambdar@
  (syntax-rules (:)
    ((_ (c) e) (lambda (c) e))
    ((_ (c : S D Y N T) e)
     (lambda (c)
       (let ((S (c->S c))
             (D (c->D c))
             (Y (c->Y c))
             (N (c->N c))
             (T (c->T c)))
         e)))))

(define tagged?
  (lambda (S Y y^)
    (exists (lambda (y) (eqv? (walk/reifier y S) y^)) Y)))

(define untyped-var?
  (lambda (S Y N t^)
    (let ((in-type? (lambda (y) (var-eq? (walk/reifier y S) t^))))
      (and (var? t^)
           (not (exists in-type? Y))
           (not (exists in-type? N))))))

(define reify-S
  (lambda (v S)
    (let ((v (walk/reifier v S)))
      (cond
        ((var? v)
         (let ((n (subst-length S)))
           (let ((name (reify-name n)))
             (subst-add S v name empty-provenance))))
        ((pair? v)
         (let ((S (reify-S (car v) S)))
           (reify-S (cdr v) S)))
        (else S)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define drop-dot
  (lambda (X)
    (map (lambda (t)
           (let ((a (lhs t))
                 (d (rhs t)))
             `(,a ,d)))
         X)))

(define sorter
  (lambda (ls)
    (list-sort lex<=? ls)))

(define lex<=?
  (lambda (x y)
    (string<=? (datum->string x) (datum->string y))))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))

(define anyvar?
  (lambda (u r)
    (cond
      ((pair? u)
       (or (anyvar? (car u) r)
           (anyvar? (cdr u) r)))
      (else (var? (walk/reifier u r))))))

(define member*
  (lambda (u v)
    (cond
      ((equal? u v) #t)
      ((pair? v)
       (or (member* u (car v)) (member* u (cdr v))))
      (else #f))))

(define drop-N-b/c-const
  (lambdar@ (c : S D Y N T)
    (let ((const? (lambda (n)
                    (not (var? (walk/reifier n S))))))
      (cond
        ((find const? N) =>
           (lambda (n) `(,S ,D ,Y ,(remq1 n N) ,T)))
        (else c)))))

(define drop-Y-b/c-const
  (lambdar@ (c : S D Y N T)
    (let ((const? (lambda (y)
                    (not (var? (walk/reifier y S))))))
      (cond
        ((find const? Y) =>
           (lambda (y) `(,S ,D ,(remq1 y Y) ,N ,T)))
        (else c)))))

(define remq1
  (lambda (elem ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) elem) (cdr ls))
      (else (cons (car ls) (remq1 elem (cdr ls)))))))

(define same-var?
  (lambda (v)
    (lambda (v^)
      (and (var? v) (var? v^) (var-eq? v v^)))))

(define find-dup
  (lambda (f S)
    (lambda (set)
      (let loop ((set^ set))
        (cond
          ((null? set^) #f)
          (else
           (let ((elem (car set^)))
             (let ((elem^ (walk/reifier elem S)))
               (cond
                 ((find (lambda (elem^^)
                          ((f elem^) (walk/reifier elem^^ S)))
                        (cdr set^))
                  elem)
                 (else (loop (cdr set^))))))))))))

(define drop-N-b/c-dup-var
  (lambdar@ (c : S D Y N T)
    (cond
      (((find-dup same-var? S) N) =>
       (lambda (n) `(,S ,D ,Y ,(remq1 n N) ,T)))
      (else c))))

(define drop-Y-b/c-dup-var
  (lambdar@ (c : S D Y N T)
    (cond
      (((find-dup same-var? S) Y) =>
       (lambda (y)
         `(,S ,D ,(remq1 y Y) ,N ,T)))
      (else c))))

(define var-type-mismatch?
  (lambda (S Y N t1^ t2^)
    (cond
      ((num? S N t1^) (not (num? S N t2^)))
      ((sym? S Y t1^) (not (sym? S Y t2^)))
      (else #f))))

(define term-ununifiable?
  (lambda (S Y N t1 t2)
    (let ((t1^ (walk/reifier t1 S))
          (t2^ (walk/reifier t2 S)))
      (cond
        ((or (untyped-var? S Y N t1^) (untyped-var? S Y N t2^)) #f)
        ((var? t1^) (var-type-mismatch? S Y N t1^ t2^))
        ((var? t2^) (var-type-mismatch? S Y N t2^ t1^))
        ((and (pair? t1^) (pair? t2^))
         (or (term-ununifiable? S Y N (car t1^) (car t2^))
             (term-ununifiable? S Y N (cdr t1^) (cdr t2^))))
        (else (not (eqv? t1^ t2^)))))))

(define T-term-ununifiable?
  (lambda (S Y N)
    (lambda (t1)
      (let ((t1^ (walk/reifier t1 S)))
        (letrec
            ((t2-check
              (lambda (t2)
                (let ((t2^ (walk/reifier t2 S)))
                  (if (pair? t2^)
                    (and
                       (term-ununifiable? S Y N t1^ t2^)
                       (t2-check (car t2^))
                       (t2-check (cdr t2^)))
                    (term-ununifiable? S Y N t1^ t2^))))))
          t2-check)))))

(define num?
  (lambda (S N n)
    (let ((n (walk/reifier n S)))
      (cond
        ((var? n) (tagged? S N n))
        (else (number? n))))))

(define sym?
  (lambda (S Y y)
    (let ((y (walk/reifier y S)))
      (cond
        ((var? y) (tagged? S Y y))
        (else (symbol? y))))))

(define drop-T-b/c-Y-and-N
  (lambdar@ (c : S D Y N T)
    (let ((drop-t? (T-term-ununifiable? S Y N)))
      (cond
        ((find (lambda (t) ((drop-t? (lhs t)) (rhs t))) T) =>
         (lambda (t) `(,S ,D ,Y ,N ,(remq1 t T))))
        (else c)))))

(define move-T-to-D-b/c-t2-atom
  (lambdar@ (c : S D Y N T)
    (cond
      ((exists (lambda (t)
               (let ((t2^ (walk/reifier (rhs t) S)))
                 (cond
                   ((and (not (untyped-var? S Y N t2^))
                         (not (pair? t2^)))
                    (let ((T (remq1 t T)))
                      `(,S ((,t) . ,D) ,Y ,N ,T)))
                   (else #f))))
             T))
      (else c))))

(define terms-pairwise=?
  (lambda (pr-a^ pr-d^ t-a^ t-d^ S)
    (or
     (and (term=? pr-a^ t-a^ S)
          (term=? pr-d^ t-a^ S))
     (and (term=? pr-a^ t-d^ S)
          (term=? pr-d^ t-a^ S)))))

(define T-superfluous-pr?
  (lambda (S Y N T)
    (lambda (pr)
      (let ((pr-a^ (walk/reifier (lhs pr) S))
            (pr-d^ (walk/reifier (rhs pr) S)))
        (cond
          ((exists
               (lambda (t)
                 (let ((t-a^ (walk/reifier (lhs t) S))
                       (t-d^ (walk/reifier (rhs t) S)))
                   (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S)))
             T)
           (for-all
            (lambda (t)
              (let ((t-a^ (walk/reifier (lhs t) S))
                    (t-d^ (walk/reifier (rhs t) S)))
                (or
                 (not (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S))
                 (untyped-var? S Y N t-d^)
                 (pair? t-d^))))
            T))
          (else #f))))))

(define drop-from-D-b/c-T
  (lambdar@ (c : S D Y N T)
    (cond
      ((find
           (lambda (d)
             (exists
                 (T-superfluous-pr? S Y N T)
               d))
         D) =>
         (lambda (d) `(,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define drop-t-b/c-t2-occurs-t1
  (lambdar@ (c : S D Y N T)
    (cond
      ((find (lambda (t)
               (let ((t-a^ (walk/reifier (lhs t) S))
                     (t-d^ (walk/reifier (rhs t) S)))
                 (mem-check t-d^ t-a^ S)))
             T) =>
             (lambda (t)
               `(,S ,D ,Y ,N ,(remq1 t T))))
      (else c))))

(define split-t-move-to-d-b/c-pair
  (lambdar@ (c : S D Y N T)
    (cond
      ((exists
         (lambda (t)
           (let ((t2^ (walk/reifier (rhs t) S)))
             (cond
               ((pair? t2^) (let ((ta `(,(lhs t) . ,(car t2^)))
                                  (td `(,(lhs t) . ,(cdr t2^))))
                              (let ((T `(,ta ,td . ,(remq1 t T))))
                                `(,S ((,t) . ,D) ,Y ,N ,T))))
               (else #f))))
         T))
      (else c))))

(define find-d-conflict
  (lambda (S Y N)
    (lambda (D)
      (find
       (lambda (d)
	 (exists (lambda (pr)
		   (term-ununifiable? S Y N (lhs pr) (rhs pr)))
		 d))
       D))))

(define drop-D-b/c-Y-or-N
  (lambdar@ (c : S D Y N T)
    (cond
      (((find-d-conflict S Y N) D) =>
       (lambda (d) `(,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define cycle
  (lambdar@ (c)
    (let loop ((c^ c)
               (fns^ (LOF))
               (n (length (LOF))))
      (cond
        ((zero? n) c^)
        ((null? fns^) (loop c^ (LOF) n))
        (else
         (let ((c^^ ((car fns^) c^)))
           (cond
             ((not (eq? c^^ c^))
              (loop c^^ (cdr fns^) (length (LOF))))
             (else (loop c^ (cdr fns^) (sub1 n))))))))))

(define mem-check
  (lambda (u t S)
    (let ((t (walk/reifier t S)))
      (cond
        ((pair? t)
         (or (term=? u t S)
             (mem-check u (car t) S)
             (mem-check u (cdr t) S)))
        (else (term=? u t S))))))

(define term=?
  (lambda (u t S)
    (let-values (((S added) (unify/reifier u t (subst-with-scope
                                         S
                                         nonlocal-scope))))
      (and S (null? added)))))

(define ground-non-<type>?
  (lambda (pred)
    (lambda (u S)
      (let ((u (walk/reifier u S)))
        (cond
          ((var? u) #f)
          (else (not (pred u))))))))

(define ground-non-symbol?
  (ground-non-<type>? symbol?))

(define ground-non-number?
  (ground-non-<type>? number?))

(define ==fail-check
  (lambda (S0 D Y N T)
    (let ([S0 (subst-with-scope S0 nonlocal-scope)])
      (cond
        ((atomic-fail-check S0 Y ground-non-symbol?) #t)
        ((atomic-fail-check S0 N ground-non-number?) #t)
        ((symbolo-numbero-fail-check S0 Y N) #t)
        ((=/=-fail-check S0 D) #t)
        ((absento-fail-check S0 T) #t)
        (else #f)))))

(define atomic-fail-check
  (lambda (S A pred)
    (exists (lambda (a) (pred (walk/reifier a S) S)) A)))

(define symbolo-numbero-fail-check
  (lambda (S A N)
    (let ((N (map (lambda (n) (walk/reifier n S)) N)))
      (exists (lambda (a) (exists (same-var? (walk/reifier a S)) N))
        A))))

(define absento-fail-check
  (lambda (S T)
    (exists (lambda (t) (mem-check (lhs t) (rhs t) S)) T)))

(define =/=-fail-check
  (lambda (S D)
    (exists (d-fail-check S) D)))

(define d-fail-check
  (lambda (S)
    (lambda (d)
      (let-values (((S added) (unify* d S)))
        (and S (null? added))))))

(define reify+
  (lambda (v R D Y N T)
    (form (walk* v R)
          (walk* D R)
          (walk* Y R)
          (walk* N R)
          (rem-subsumed-T (walk* T R)))))

(define form
  (lambda (v D Y N T)
    (let ((fd (sort-D D))
          (fy (sorter Y))
          (fn (sorter N))
          (ft (sorter T)))
      (let ((fd (if (null? fd) fd
                    (let ((fd (drop-dot-D fd)))
                      `((=/= . ,fd)))))
            (fy (if (null? fy) fy `((sym . ,fy))))
            (fn (if (null? fn) fn `((num . ,fn))))
            (ft (if (null? ft) ft
                    (let ((ft (drop-dot ft)))
                      `((absento . ,ft))))))
        (cond
          ((and (null? fd) (null? fy)
                (null? fn) (null? ft))
           v)
          (else (append `(,v) fd fn fy ft)))))))

(define sort-D
  (lambda (D)
    (sorter
     (map sort-d D))))

(define sort-d
  (lambda (d)
    (list-sort
       (lambda (x y)
         (lex<=? (car x) (car y)))
       (map sort-pr d))))

(define drop-dot-D
  (lambda (D)
    (map drop-dot D)))

(define lex<-reified-name?
  (lambda (r)
    (char<?
     (string-ref
      (datum->string r) 0)
     #\_)))

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
        ((lex<-reified-name? r) pr)
        ((lex<=? r l) `(,r . ,l))
        (else pr)))))

(define rem-subsumed
  (lambda (D)
    (let rem-subsumed ((D D) (d^* '()))
      (cond
        ((null? D) d^*)
        ((or (subsumed? (car D) (cdr D))
             (subsumed? (car D) d^*))
         (rem-subsumed (cdr D) d^*))
        (else (rem-subsumed (cdr D)
                (cons (car D) d^*)))))))

(define subsumed?
  (lambda (d d*)
    (cond
      ((null? d*) #f)
      (else
        (let-values (((S ignore) (unify* d (subst
                                             empty-subst-map
                                             nonlocal-scope))))
          (let-values (((S+ added) (unify* (car d*) S)))
            (or
              (and S+ (null? added))
              (subsumed? d (cdr d*)))))))))



(define rem-xx-from-d
  (lambdar@ (c : S D Y N T)
    (let ((D (walk* D S)))
      (remp not
            (map (lambda (d)
                   (let-values (((S0 ignore) (unify* d S)))
                     (cond
                       ((not S0) #f)
                       ((==fail-check S0 '() Y N T) #f)
                       (else
                         (let-values
                           (((S added)
                             (unify* d (subst empty-subst-map
                                              nonlocal-scope))))
                           added)))))
                 D)))))

(define rem-subsumed-T
  (lambda (T)
    (let rem-subsumed ((T T) (T^ '()))
      (cond
        ((null? T) T^)
        (else
         (let ((lit (lhs (car T)))
               (big (rhs (car T))))
           (cond
             ((or (subsumed-T? lit big (cdr T))
                  (subsumed-T? lit big T^))
              (rem-subsumed (cdr T) T^))
             (else (rem-subsumed (cdr T)
                     (cons (car T) T^))))))))))

(define subsumed-T?
  (lambda (lit big T)
    (cond
      ((null? T) #f)
      (else
       (let ((lit^ (lhs (car T)))
             (big^ (rhs (car T))))
         (or
           (and (eq? big big^) (member* lit^ lit))
           (subsumed-T? lit big (cdr T))))))))

(define LOF
  (lambda ()
    `(,drop-N-b/c-const ,drop-Y-b/c-const ,drop-Y-b/c-dup-var
      ,drop-N-b/c-dup-var ,drop-D-b/c-Y-or-N ,drop-T-b/c-Y-and-N
      ,move-T-to-D-b/c-t2-atom ,split-t-move-to-d-b/c-pair
      ,drop-from-D-b/c-T ,drop-t-b/c-t2-occurs-t1)))


; simple-mk

;(define reify-s
  ;(lambda (v s)
    ;(let-values (((v _) (walk v s)))
      ;(cond
        ;((var? v)
         ;(ext-s v (reify-name (subst-length s)) s '()))
        ;((pair? v) (reify-s (cdr v)
                     ;(reify-s (car v) s)))
        ;(else s)))))

;(define reify-name
  ;(lambda (n)
    ;(string->symbol
      ;(string-append "_" "." (number->string n)))))

;(define reify
  ;(lambda (v)
    ;(lambda (st)
      ;(when (or (log-stats))
        ;(printf "state ctr: ~a\n" (state-counter st)))
      ;(let ([st (state-with-scope st nonlocal-scope)])
        ;(let ((s (state-s st)))
          ;(let ((v (walk* v s)))
            ;(walk* v (reify-s v (subst-with-scope empty-subst nonlocal-scope)))))))))

;;; Search

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

(define (hard-reset!)
  (sat/hard-reset!)

  (soft-reset!))

(define (soft-reset!)
  (set! unification-count 0)
  (set! assumption-count 0)
  (reset-sat-counts!)

  (sat/soft-reset!))

(hard-reset!)

