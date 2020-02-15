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
    (if (scope-eq? (var-scope x) (subst-scope S))
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


;;(define smt-cmd "cvc4 --lang=smt2.6 -m --incremental --fmf-fun")
(define smt-cmd "z3 -in -t:20")

(define-values (smt-out smt-in smt-err smt-p) (values #f #f #f #f))
(define (smt-reset!)
  (let-values (((out in err p)
                (process/text-ports smt-cmd)))
    (set! smt-out out)
    (set! smt-in in)
    (set! smt-err err)
    (set! smt-p p)))

(define (smt-read-sat)
  (let ([r (read smt-in)])
    ;(printf "read sat>> ~a\n" r)
    (cond
      ((eq? r 'sat)
         ;(printf "read-sat: sat\n")
       #t)
      ((eq? r 'unsat)
         ;(printf "read-sat: unsat\n")
       #f)
      ((eq? r 'unknown)
       (begin
         (printf "read-sat: unknown\n")
         (printf "assumptions: ~a\n" assumption-count)
         #t))
      (else (error 'read-sat (format "~a" r))))))

(define buffer '())
(define (smt-call xs)
  (set! buffer (cons xs buffer)))

(define (smt-flush!)
  (let ([buffered (reverse buffer)])
    (for-each
      (lambda (stmts)
        (for-each
          (lambda (x)
            ;(printf "~s\n" x)
            ;(flush-output-port)
            (fprintf smt-out "~s\n" x))
          stmts))
      buffered))
  (flush-output-port smt-out)
  (set! buffer '()))

(define (smt-call/flush xs)
  (smt-call xs)
  (smt-flush!))

;; State: (Counter, Substitution, AssertionHistory)
(define (state c s ah) (list c s ah))
(define state-counter car)
(define state-s cadr)
(define state-assertion-history caddr)
(define (state-s-set st s)
  (state (state-counter st) s (state-assertion-history st)))
(define (extend-assertion-history st ctx)
  (state (state-counter st) (state-s st)
         (cons ctx (state-assertion-history st))))
(define state-with-scope
  (lambda (st new-scope)
    (state (state-counter st)
           (subst-with-scope (state-s st) new-scope)
           (state-assertion-history st))))
;; AssertionHistory: (AssumptionVariableId, SMT_Statements)
;; Substitution: AList from Variable to (Term,ProvenanceSet)
;; ProvenanceSet: List of AssumptionVariableId
(define empty-assertion-history '())
(define empty-state `(0 ,empty-subst ,empty-assertion-history))

;; a set of asserted assumption variable ids
(define empty-seen-assumptions make-eq-hashtable)
(define seen-assumptions (empty-seen-assumptions))
(define (saw-assumption! id)
  (hashtable-set! seen-assumptions id #t))
(define (seen-assumption? id)
  (hashtable-contains? seen-assumptions id))
(define (assumption-id->symbol id)
  (string->symbol (format "_a~a" id)))
(define assumption-count 0)
(define (fresh-assumption-id!)
  (set! assumption-count (+ 1 assumption-count))
  (let ([id (assumption-id->symbol assumption-count)])
    (smt-call `((declare-const ,id Bool)))
    id))
;(define assumption-id-sym cdr)
;(define assumption-id-int car)
(define empty-child-assumptions make-eq-hashtable)
(define child-assumptions (empty-child-assumptions))
(define (get-child-assumptions! id)
  (let ((r (hashtable-ref child-assumptions id #f)))
    (if r
        (values (car r) (cdr r))
        (let ([l (fresh-assumption-id!)] [r (fresh-assumption-id!)])
          (hashtable-set! child-assumptions id (cons l r))
          (values l r)))))

(define (smt/add ctx stmt st)
  (smt-call (list stmt))
  (extend-assertion-history st ctx))
(define (smt/add-if-new ctx stmt st)
  (unless (seen-assumption? ctx)
    (saw-assumption! ctx)
    (smt-call (list stmt)))
  ;; may have seen the assumption along a different search path
  ;; so updating always
  (extend-assertion-history st ctx))

;; Counter: Integer (used to decide whether to actually call the solver)
(define (inc-counter st)
  (cons (+ 1 (car st)) (cdr st)))
(define get-counter car)

(define smt/check-sometimes
  (lambda (st)
    (let ((st (inc-counter st)))
      ;(smt/check st)
      ;st
      (if (= (remainder (get-counter st) 200) 0)
          (smt/check st)
          st))))

(define smt/check
  (lambda (st)
    (smt-call/flush
      `((check-sat-assuming
          ,(state-assertion-history st))))
    (if (smt-read-sat)
        st
        #f)))

(define (smt/assert e ctx st)
  (smt/check-sometimes
    (smt/add-if-new ctx `(assert (= ,ctx ,e)) st)))

(define (smt/assert-leaf ctx st)
  (extend-assertion-history st ctx))

(define (smt/conflict prov st)
  ;; OK to be ephemeral, only boost
  (smt-call (list `(assert (not (and . ,prov)))))
  #f)

(define smt/purge
  (lambda (ctx)
    (lambda (st) st)
    #;smt/check))

(define (smt/reset!)
  (set! assumption-count 0)
  (set! seen-assumptions (empty-seen-assumptions))
  (set! child-assumptions (empty-child-assumptions))
  (smt-call/flush '((reset))))


(define rhs
  (lambda (pair)
    (cdr pair)))

(define lhs
  (lambda (pair)
    (car pair)))

(define size-s
  subst-map-length)

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
         (ext-s v (reify-name (size-s s)) s '()))
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



(define (prov-from-ctx ctx) (list ctx))
(define (== u v)
  (lambda (ctx)
    (lambda (st)
      (let-values
          (((success? s)
            (unify-check u v (state-s st) (prov-from-ctx ctx))))
        (if success?
            (smt/assert-leaf ctx (state-s-set st s))
            (smt/conflict s st))))))

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

(define (assert-and ctx1 ctx2 ctx st)
  (smt/assert `(and ,ctx1
                    ,ctx2)
              ctx st))

(define (assert-or ctx1 ctx2 ctx st)
  (smt/assert `(or ,ctx1
                   ,ctx2)
              ctx st))

; -> Goal
(define (conj2 ig1 ig2)
     (lambda (ctx)
       (let-values (((ctx1 ctx2) (get-child-assumptions! ctx)))
         (lambdag@ (st)
                   (let ([st (assert-and ctx1 ctx2 ctx st)])
                     (and st (bind ((ig1 ctx1) st) (ig2 ctx2))))))))

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
       (let-values (((ctx1 ctx2) (get-child-assumptions! ctx)))
         (lambdag@ (st)
                   (let ([st (assert-or ctx1 ctx2 ctx st)])
                     (and st (mplus ((ig1 ctx1) st) (inc (((ig2) ctx2) st)))))))))

(define-syntax disj*
  (syntax-rules ()
    ((_ ig) ig)
    ((_ ig0 ig ...) (disj2 ig0 (lambda () (disj* ig ...))))))

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
#;
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       (inc
         (mplus*
          (bind* (g0 st) g ...)
          (bind* (g1 st) g^ ...) ...))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) ig ...)
     (begin
       (smt/reset!)
       (let ((ctx (fresh-assumption-id!)))
         (let ((q (var initial-scope)))
           (map (reify q)
                (take n
                      (inc
                       (((conj* ig ... smt/purge) ctx)
                        empty-state))))))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

(define fail (lambda (ctx) (lambda (st) #f)))
(define succeed (lambda (ctx) (lambda (st) st)))

(smt-reset!)
