;;; allw tests

(define-syntax allw
  (syntax-rules ()
   [(_ e ...) (conj* e ...)]))

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (cout "Testing " title nl)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
    args))

(define errorf
  (lambda (tag . args)
    (display "Failed: ") (display tag) (newline)
    (for-each  display args)
    (error 'WiljaCodeTester "That's all, folks!")))

(define-syntax test-divergence
  (syntax-rules ()
    ((_ title tested-expression)
     (let ((max-ticks 1000000))
       (cout "Testing " title " (engine with " max-ticks " ticks fuel)" nl)
       ((make-engine (lambda () tested-expression))
        max-ticks
        (lambda (t v)
          (errorf title
            "infinite loop returned " v " after " (- max-ticks t) " ticks"))
        (lambda (e^) (void)))))))

;;; Comment out this definition to test divergent code (Chez Scheme only)
'(define-syntax test-divergence
  (syntax-rules ()
    ((_ title tested-expression) (cout "Ignoring divergent test " title nl))))


(define succeed (== #t #t))
(define fail (== #t #f))


(define anyo
  (lambda (g)
    (conde
      (g succeed)
      ((anyo g)))))

(define nevero (anyo fail))

(define alwayso (anyo succeed))

(define nullo
  (lambda (x)
    (== '() x)))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define eqo
  (lambda (x y)
    (== x y)))

(define +o
  (lambda (n m k)
    (addero 0 n m k)))

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(define addero
  (lambda (d n m r)
    (condi
      ((== 0 d) (== '() m) (== n r))
      ((== 0 d) (== '() n) (== m r)
       (poso m))
      ((== 1 d) (== '() m)
       (addero 0 n '(1) r))
      ((== 1 d) (== '() n) (poso m)
       (addero 0 '(1) m r))
      ((== '(1) n) (== '(1) m)
       (fresh (a c)
         (== `(,a ,c) r)
         (full-addero d 1 1 a c)))
      ((== '(1) n) (gen-addero d n m r))
      ((== '(1) m) (>1o n) (>1o r)
       (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r)))))

(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== `(,a ,ad . ,dd) n))))

(define full-addero
  (lambda (b x y r c)
    (conde
      ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c)))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (alli
        (full-addero d a b c e)
        (addero e x y z)))))

(test-check "allw-1"
  (run* (q)
    (allw
      succeed
      (conde
        ((== q 1))
        ((== q 2)))))
  '(1 2))

(test-check "allw-2"
  (run* (q)
    (fresh (x y)
      (allw
        (conde
          ((== x 1) (== y 2))
          ((== x 2) (== y 1)))
        (== q `(,x ,y)))))
  '((1 2) (2 1)))

(test-check "allw-3"
  (run* (q)
    (fresh (x y)
      (allw 
        (== q `(,x ,y))
        (conde
          ((== x 1) (== y 2))
          ((== x 2) (== y 1))))))
  '((1 2) (2 1)))


;(test-check "allw-4"
  ;(run* (q)
    ;(fresh (x y)
      ;(allw
        ;nevero
        ;fail)))
  ;'())

(test-check "allw-5"
  (run* (q)
    (fresh (x y)
      (allw 
        fail
        nevero)))
  '())

;(test-check "allw-6"
  ;(run* (q)
    ;(fresh (x y)
      ;(allw 
        ;nevero
        ;(allw
          ;nevero
          ;fail))))
  ;'())

(test-divergence "allw-7"
  (run* (q)
    (fresh (x y)
      (allw 
        nevero
        nevero))))

(test-divergence "allw-8"
  (run* (q)
    (fresh (x y)
      (allw 
        nevero
        alwayso))))

(test-divergence "allw-9"
  (run* (q)
    (fresh (x y)
      (allw 
        alwayso
        nevero))))

(test-check "allw-10"
  (run* (q)
    (fresh (x y)
      (allw 
        alwayso
        fail)))
  '())

;;; Ron's tests
(test-check "allw-11"
  (run* (q)
    (allw
      (== q 5)
      succeed))
  '(5))

(test-check "allw-12"
  (run* (q)
    (allw
      (== 5 5)
      (== q 6)))
  '(6))

(test-check "allw-13"
  (run* (q)
    (allw
      fail
      nevero))
  '())

;(test-check "allw-14"
  ;(run* (q)
    ;(allw
      ;nevero
      ;fail))
  ;'())

;(test-check "allw-15a"
  ;(run* (q)
    ;(allw
      ;(letrec
        ;((anyo (lambda (g)
                 ;(conde
                   ;(g succeed)
                   ;((anyo g))))))
        ;(anyo fail))
      ;fail))
  ;'())

;(test-check "allw-15b"
  ;(run* (q)
    ;(allw
      ;(anyo fail)
      ;fail))
  ;'())

;;; Bad test
;;; Error: variable g is not bound.
'(test-check "allw-16"
  ((lambda (n q)
     (if ((lambda (t) (if t t (> n 0))) (not n))
       (map-inf
         n
         (lambda (s) (reify (walk* q s)))
         ((((lambda (g^)
              (lambda (s) (bindw (g^ s) (lambda (s) (fail s)))))
            ((letrec
               ((anyo
                  (lambda (s)
                    (lambda (p)
                      (mplus
                        ((((lambda (g^)
                             (lambda (s)
                               (bindw
                                 (g^ s)
                                 (lambda (s) (succeed s)))))
                           g)
                          s)
                         p)
                        (lambda () (((anyo g) s) p)))))))
               anyo)
             fail))
           empty-s)
          rebind))
       ()))
   #f
   (var 'q))
  '())

;(define neverw (rec neverw (allw succeed neverw)))

;(test-check "allw-17"
  ;(run* (q)
    ;(allw
      ;neverw
      ;fail))
  ;'())

;(test-divergence "allw-18"
  ;(run* (q)
    ;neverw))

;(test-divergence "allw-19"
  ;(run* (q)
    ;neverw
    ;neverw
    ;neverw
    ;fail))

;(test-check "allw-20"
  ;(run* (q)
    ;(allw
      ;neverw
      ;(allw
        ;neverw
        ;(allw
          ;neverw
          ;fail))))
  ;'())

;(test-check "allw-21"
  ;(run* (q)
    ;(allw
      ;neverw
      ;(allw
        ;neverw
        ;(allw
          ;fail
          ;neverw))))
  ;'())

;(test-check "allw-22"
  ;(run* (q)
    ;(allw
      ;neverw
      ;(allw
        ;fail
        ;(allw
          ;neverw
          ;neverw))))
  ;'())

;(test-check "allw-23"
  ;(run* (q)
    ;(allw
      ;fail
      ;(allw
        ;neverw
        ;(allw
          ;neverw
          ;neverw))))
  ;'())

;(test-check "allw-24"
  ;(run* (q)
    ;(allw
      ;nevero
      ;(allw
        ;nevero
        ;(allw
          ;nevero
          ;fail))))
  ;'())

;(test-check "allw-25"
  ;(run* (q)
    ;(allw
      ;nevero
      ;(allw
        ;nevero
        ;(allw
          ;fail
          ;nevero))))
  ;'())

;(test-check "allw-26"
  ;(run* (q)
    ;(allw
      ;nevero
      ;(allw
        ;fail
        ;(allw
          ;nevero
          ;nevero))))
  ;'())

;(test-divergence "allw-27"
  ;(run* (q)
    ;(all
      ;nevero
      ;fail)))

;(test-divergence "allw-28"
  ;(run* (q)
    ;(all
      ;nevero
      ;fail
      ;nevero
      ;nevero)))

;(test-check "allw-29"
  ;(run* (q)
    ;(allw
      ;nevero
      ;(allw
        ;fail
        ;(allw
          ;neverw
          ;nevero))))
  ;'())

;;; Byrd 35

(define foo
  (lambda (x)
    (allw
      (== 5 x)
      (foo x))))

(define bar
  (lambda (x)
    (allw
      (bar x)
      (== 5 x))))

(define quux
  (lambda (x)
    (allw
      (== 5 x)
      (allw
        (quux x)
        (quux x)))))

(define quuux
  (lambda (x)
    (allw
      (quuux x)
      (allw
        (quuux x)
        (== 5 x)))))

(define blat
  (lambda (x)
    (allw
      (allw
        (blat x)
        (blat x))
      (== 5 x))))

(test-check "allw-30"
  (run* (q) (allw fail alwayso))
  '())

(test-check "allw-31"
  (run* (q) (allw (== q 3) (== q 3)))
  '(3))

(test-check "allw-32"
  (run* (q) (allw (== q 3) (== q 4)))
  '())

(test-check "foo-1"
  (run 1 (q)
    (foo 6))
  '())

(test-check "foo-2"
  (run* (q)
    (foo 6))
  '())

(test-check "bar-1"
  (run 1 (q)
    (bar 6))
  '())

(test-check "quux-1"
  (run* (q)
    (quux 6))
  '())

(test-check "quuux-1"
  (run 1 (q)
    (quuux 6))
  '())

(test-check "blat-1"
  (run* (q)
    (blat 6))
  '())

(define appendw
  (lambda (l1 l2 out)
    (conde
      ((allw (nullo l1) (== l2 out)))
      ((fresh (a d res)
         (allw
           (allw
             (conso a d l1)
             (conso a res out))
           (appendw d l2 res)))))))

(define appendw
  (lambda (l1 l2 out)
    (conde
      ((all (nullo l1) (== l2 out)))
      ((fresh (a d res)
         (allw
           (all
             (conso a d l1)
             (conso a res out))
           (appendw d l2 res)))))))

(test-check "appendw-0"
  (run* (q)
    (appendw '(a b c) '(d e) q))
  '((a b c d e)))

(test-check "appendw-1"
  (run 1 (q)
    (appendw '(a b c) '(d e) q))
  '((a b c d e)))

(test-check "appendw-1"
  (run 5 (x)
    (fresh (y z)
      (appendw x y z)))
  '(()
    (_.0)
    (_.0 _.1)
    (_.0 _.1 _.2)
    (_.0 _.1 _.2 _.3)))

(test-check "appendw-2"
  (run 5 (q)
    (fresh (x y z)
      (appendw x y z)
      (== `(,x ,y ,z) q)))
  '((() _.0 _.0)
    ((_.0) _.1 (_.0 . _.1))
    ((_.0 _.1) _.2 (_.0 _.1 . _.2))
    ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
    ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))

(define assqo
  (lambda (x ls out)
    (conde
      ((fresh (a aa)
         (allw
           (allw
             (caro ls a)
             (caro a aa))
           (eqo aa x)))
       (caro ls out))
      ((fresh (d)
         (allw
           (cdro ls d)
           (assqo x d out)))))))

(test-check "assqo-forward-1"
  (run* (q)
    (assqo 'x '((x . 5)) q))
  '((x . 5)))

(test-check "assqo-forward-2"
  (run* (q)
    (assqo 'x '() q))
  '())

(test-check "assqo-forward-3"
  (run* (q)
    (assqo 'x '((w . 3) (x . 5) (y . 7)) q))
  '((x . 5)))

(test-check "assqo-forward-4"
  (run* (q)
    (assqo 'x '((y . #t) (x . 3) (x . 5) (z . #f) (x . 7)) q))
  '((x . 3) (x . 5) (x . 7)))

(test-check "assqo-backward-1"
  (run 5 (q)
    (fresh (sym ls)
      (allw
        (assqo sym ls '(x . 3))
        (== `(,sym ,ls) q))))
  '((x ((x . 3) . _.0))
    (x (_.0 (x . 3) . _.1))
    (x (_.0 _.1 (x . 3) . _.2))
    (x (_.0 _.1 _.2 (x . 3) . _.3))
    (x (_.0 _.1 _.2 _.3 (x . 3) . _.4))))

(test-check "assqo-backward-2"
  (run 5 (q)
    (fresh (sym ls v)
      (assqo sym ls `(x . ,v))
      (== `(,sym ,ls) q)))
  '((x ((x . _.0) . _.1))
    (x (_.0 (x . _.1) . _.2))
    (x (_.0 _.1 (x . _.2) . _.3))
    (x (_.0 _.1 _.2 (x . _.3) . _.4))
    (x (_.0 _.1 _.2 _.3 (x . _.4) . _.5))))

(test-check "assqo-generate"
  (run 5 (q)
    (fresh (sym ls out)
      (assqo sym ls out)
      (== `(,sym ,ls ,out) q)))
  '((_.0 ((_.0 . _.1) . _.2) (_.0 . _.1))
    (_.0 (_.1 (_.0 . _.2) . _.3) (_.0 . _.2))
    (_.0 (_.1 _.2 (_.0 . _.3) . _.4) (_.0 . _.3))
    (_.0 (_.1 _.2 _.3 (_.0 . _.4) . _.5) (_.0 . _.4))
    (_.0 (_.1 _.2 _.3 _.4 (_.0 . _.5) . _.6) (_.0 . _.5))))

(printf "1\n")

(define lengtho
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (cdro ls d)
         (lengtho d res)
         (+o res '(1) out))))))

(test-check "lengtho-forward-1"
  (run* (q)
    (lengtho '(a b c) q))
  '((1 1)))

(test-check "lengtho-forward-2"
  (run* (q)
    (lengtho '() q))
  '(()))

(test-check "lengtho-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengtho ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

(test-divergence "lengtho-backward-1"
  (run* (q)
    (lengtho q '(1 1))))

(test-divergence "lengtho-backward-2"
  (run* (q)
    (lengtho q '())))

(printf "2\n")

;;; swap the last two goals
(define lengtho
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (cdro ls d)
         (+o res '(1) out)
         (lengtho d res))))))

(test-divergence "lengtho-forward-1"
  (run* (q)
    (lengtho '(a b c) q)))

(test-check "lengtho-forward-2"
  (run* (q)
    (lengtho '() q))
  '(()))

(test-check "lengtho-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengtho ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

(test-check "lengtho-backward-1"
  (run* (q)
    (lengtho q '(1 1)))
  '((_.0 _.1 _.2)))

(test-check "lengtho-backward-2"
  (run* (q)
    (lengtho q '()))
  '(()))

(printf "3\n")

(define lengthw
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (cdro ls d)
         (allw
           (+o res '(1) out)
           (lengthw d res)))))))

;;; badness? (not anymore)
(test-check "lengthw-forward-1"
  (run* (q)
    (lengthw '(a b c) q))
  '((1 1))
  )

(test-check "lengthw-forward-1a"
  (run 1 (q)
    (lengthw '(a b c) q))
  '((1 1)))

(test-check "lengthw-forward-2"
  (run* (q)
    (lengthw '() q))
  '(()))

(test-check "lengthw-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengthw ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

;;; Should this really diverge?
(test-check "lengthw-backward-1"
  (run* (q)
    (lengthw q '(1 1)))
  '((_.0 _.1 _.2)))

;;; Should this really diverge?
(test-check "lengthw-backward-2"
  (run* (q)
    (lengthw q '()))
  '(()))

(printf "4\n")

;;; swap goals within 'allw'
(define lengthw
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (cdro ls d)
         (allw
           (lengthw d res)
           (+o res '(1) out)))))))

(test-check "lengthw-forward-1"
  (run* (q)
    (lengthw '(a b c) q))
  '((1 1)))

(test-check "lengthw-forward-1a"
  (run 1 (q)
    (lengthw '(a b c) q))
  '((1 1)))

(test-check "lengthw-forward-2"
  (run* (q)
    (lengthw '() q))
  '(()))

(test-check "lengthw-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengthw ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

;;; Should this really diverge? (it does again)
(test-divergence "lengthw-backward-1"
  (run* (q)
    (lengthw q '(1 1))))
  ;'((_.0 _.1 _.2)))

;;; Should this really diverge?
(test-check "lengthw-backward-2"
  (run* (q)
    (lengthw q '()))
  '(()))

(printf "5\n")

(define lengthw
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (cdro ls d)
         (allw
           (+o res '(1) out)
           (lengthw d res)))))))

;;; badness??? (not anymore)
(test-check "lengthw-forward-1"
  (run* (q)
    (lengthw '(a b c) q))
    '((1 1))
  )

(test-check "lengthw-forward-1a"
  (run 1 (q)
    (lengthw '(a b c) q))
  '((1 1)))

(test-check "lengthw-forward-2"
  (run* (q)
    (lengthw '() q))
  '(()))

(test-check "lengthw-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengthw ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

;;; Should this diverge?
(test-check "lengthw-backward-1"
  (run* (q)
    (lengthw q '(1 1)))
  '((_.0 _.1 _.2)))

;;; Should this diverge?
(test-check "lengthw-backward-2"
  (run* (q)
    (lengthw q '()))
  '(()))

(printf "6\n")

;;; play more games
(define lengthw
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (allw
           (lengthw d res)
           (cdro ls d))
         (+o res '(1) out))))))

;;; badness???
(test-divergence "lengthw-forward-1"
  (run* (q)
    (lengthw '(a b c) q))
  ;'((1 1))
  )

(test-check "lengthw-forward-1a"
  (run 1 (q)
    (lengthw '(a b c) q))
  '((1 1)))

(test-check "lengthw-forward-2"
  (run* (q)
    (lengthw '() q))
  '(()))

(test-check "lengthw-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengthw ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

(test-divergence "lengthw-backward-1"
  (run* (q)
    (lengthw q '(1 1))))

(test-divergence "lengthw-backward-2"
  (run* (q)
    (lengthw q '())))

(printf "7\n")

;;; Try this will 'all'
(define lengthw
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (all
           (lengthw d res)
           (cdro ls d))
         (+o res '(1) out))))))

(test-divergence "lengthw-forward-1"
  (run* (q)
    (lengthw '(a b c) q)))

(test-divergence "lengthw-forward-2"
  (run* (q)
    (lengthw '() q)))

(test-check "lengthw-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengthw ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

(test-divergence "lengthw-backward-1"
  (run* (q)
    (lengthw q '(1 1))))

(test-divergence "lengthw-backward-2"
  (run* (q)
    (lengthw q '())))

(test-check "okay"
  (run 1 (q)
     (fresh (n m)
       (allw
         (== n m)
        (+o n '(1) m))))
  '())

(test-check "goodness"
  (run 1 (q)
    (fresh (n m)
      (allw
        (+o n '(1) m)
        (== n m))))
  '())

(test-check "goodness2"
  (run* (q)
    (fresh (n m)
      (allw
        (+o n '(1) m)
        (== n m))))
  '())

(test-check "goodness3"
  (run* (q)
     (fresh (n m)
       (allw
         (== n m)
        (+o n '(1) m))))
  '())

(define foo
  (lambda ()
    (allw fail (foo))))

(test-check "foo"
  (run* (q)
    (foo))
  '())

(define bar
  (lambda ()
    (allw (bar) fail)))

(test-check "bar"
  (run* (q)
    (bar))
  '())

(define baz
  (lambda (z)
    (fresh (x)
      (all
        (+o x '(1) z)
        (baz z)))))

(test-check "baz"
  (run* (q)
    (baz '()))
  '())

(define bazw
  (lambda (z)
    (fresh (x)
      (allw
        (+o x '(1) z)
        (bazw z)))))

(test-check "bazw"
  (run* (q)
    (bazw '()))
  '())

(define quuxw
  (lambda (z)
    (fresh (x y)
      (allw
        (+o x '(1) z)
        (quuxw y)))))

(test-check "quuxw"
  (run* (q)
    (quuxw '()))
  '())

(define bazz
  (lambda (z)
    (fresh (x)
      (all
        (bazz z)
        (+o x '(1) z)))))

(test-divergence "bazz"
  (run* (q)
    (bazz '())))

(define bazzw
  (lambda (z)
    (fresh (x)
      (allw
        (bazzw z)
        (+o x '(1) z)))))

(test-check "bazzw"
  (run* (q)
    (bazzw '()))
  '())

(define blat
  (lambda (ls out)
    (fresh (d res)
      (cdro ls d)
      (all
        (+o res '(1) out)
        (blat d res)))))

(test-check "blat"
  (run* (q)
    (blat q '()))
  '())

(define blaz
  (lambda (out)
    (fresh (res)
      (all
        (+o res '(1) out)
        (blaz res)))))

(test-check "blaz"
  (run* (q)
    (blaz '()))
  '())

(define blaz2
  (lambda (out)
    (fresh (res)
      (all
        (blaz2 res)
        (+o res '(1) out)))))

(test-divergence "blaz2"
  (run* (q)
    (blaz2 '())))

(define blazw
  (lambda (out)
    (fresh (res)
      (allw
        (+o res '(1) out)
        (blazw res)))))

(test-check "blazw"
  (run* (q)
    (blazw '()))
  '())

(define blazw
  (lambda (ls out)
    (fresh (d res)
      (allw
        (+o res '(1) out)
        (blazw d res)))))

(test-check "blazw"
  (run* (q)
    (blazw q '()))
  '())

(define blazw
  (lambda (ls out)
    (fresh (d res)
      (cdro ls d)
      (allw
        (+o res '(1) out)
        (blazw d res)))))

;;; Can the 'cdro; really be causing divergence????
(test-check "blazw"
  (run* (q)
    (blazw q '()))
  '())

(define confusing
  (lambda (ls out)
    (fresh (d res)
      succeed
      (allw
        (+o res '(1) out)
        (confusing d res)))))

;;; wtf!!  The 'succeed' before the 'allw' seems to be causing divergence!
(test-check "confusing"
  (run* (q)
    (confusing q '()))
  '())

(define confusw
  (lambda (ls out)
    (fresh (d res)
      (allw
        succeed
        (allw
          (+o res '(1) out)
          (confusw d res))))))

;;; Terminates when the 'succeed' is included in an 'allw'!
;;; What is going on here????
(test-check "confusw"
  (run* (q)
    (confusw q '()))
  '())

(printf "8\n")

;;; Let's try this again, with 'cdro' within an 'allw'.
(define lengthw
  (lambda (ls out)
    (conde
      ((nullo ls) (== '() out))
      ((fresh (d res)
         (allw
           (cdro ls d)
           (allw
             (+o res '(1) out)
             (lengthw d res))))))))

;;; badness???
(test-divergence "lengthw-forward-1"
  (run* (q)
    (lengthw '(a b c) q))
  ;'((1 1))
  )

;;; extra badness???
(test-divergence "lengthw-forward-1a"
  (run* (q)
    (lengthw '(a b c) q))
  ;'((1 1))
  )

(test-check "lengthw-forward-2"
  (run* (q)
    (lengthw '() q))
  '(()))

(test-check "lengthw-generate"
  (run 5 (q)
    (fresh (ls out)
      (lengthw ls out)
      (== `(,ls ,out) q)))
  '((() ())
    ((_.0) (1))
    ((_.0 _.1) (0 1))
    ((_.0 _.1 _.2) (1 1))
    ((_.0 _.1 _.2 _.3) (0 0 1))))

(test-check "lengthw-backward-1"
  (run* (q)
    (lengthw q '(1 1)))
  '((_.0 _.1 _.2)))

(test-check "lengthw-backward-1a"
  (run 1 (q)
    (lengthw q '(1 1)))
  '((_.0 _.1 _.2)))

(test-check "lengthw-backward-2"
  (run* (q)
    (lengthw q '()))
  '(()))

(define fail-immediately
  (conde
    (fail)))

(define fail-almost-immediately
  (conde
    (fail)
    (fail)))

(define yikes0
  (lambda (ls out)
    (fresh (d res)
      succeed
      (allw
        fail-almost-immediately
        (yikes0 d res)))))

(test-check "yikes0"
  (run* (q)
    (yikes0 q '()))
  '())

(define yikes0-fi
  (lambda (ls out)
    (fresh (d res)
      succeed
      (allw
        fail-immediately
        (yikes0-fi d res)))))

(test-check "yikes0"
  (run* (q)
    (yikes0-fi q '()))
  '())

(define yikes1
  (lambda (ls out)
    (fresh (d res)
      (all
        succeed
        (allw
          fail-almost-immediately
          (yikes1 d res))))))

(test-check "yikes1"
  (run* (q)
    (yikes1 q '()))
  '())

(define yikes1-fi
  (lambda (ls out)
    (fresh (d res)
      (all
        succeed
        (allw
          fail-immediately
          (yikes1-fi d res))))))

(test-check "yikes1-fi"
  (run* (q)
    (yikes1-fi q '()))
  '())

(define yikes2
  (lambda (ls out)
    (fresh (d res)
      (allw
        succeed
        (allw
          fail-almost-immediately
          (yikes2 d res))))))

(test-check "yikes2"
  (run* (q)
    (yikes2 q '()))
  '())
