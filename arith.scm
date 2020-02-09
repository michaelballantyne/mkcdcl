;;; Generated at 2007-10-25 15:24:42

(load "helper.scm")
(load "mk.scm")


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

(define max-ticks 10)
;;; Will sez:  Uncomment the following line to properly test divergent code.
;;; (define max-ticks 10)
(define max-ticks 10)
;;; Will sez:  Uncomment the following line to properly test divergent code.
;;; (define max-ticks 10000000)

(define-syntax test-divergence
  (syntax-rules ()
    ((_ title tested-expression)
     (let ((max-ticks 1000000))
       (printf "Testing ~s (engine with ~s ticks fuel)\n" title max-ticks)
       ((make-engine (lambda () tested-expression))
        max-ticks
        (lambda (t v)
	  (error title "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
        (lambda (e^) (void)))))))


;;; Redefine 'test-check' to make the file load quickly.
'(define-syntax test-check
(syntax-rules ()
((_ title tested-expression expected-result)
(if #f #f))))

(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define errorf
  (lambda (tag . args)
    (printf "Failed: ~s: ~%" tag)
    (apply printf args)
    (error 'WiljaCodeTester "That's all, folks!")))

;;;  Max fuel for engines
(define max-ticks 10)
;;; Will sez:  Uncomment the following line to properly test divergent code.
;;; (define max-ticks 10000000)
  

(define-syntax run1 (syntax-rules () ((_ (x) g0 g ...) (run 1 (x) g0 g ...))))
(define-syntax run2 (syntax-rules () ((_ (x) g0 g ...) (run 2 (x) g0 g ...))))
(define-syntax run3 (syntax-rules () ((_ (x) g0 g ...) (run 3 (x) g0 g ...))))
(define-syntax run4 (syntax-rules () ((_ (x) g0 g ...) (run 4 (x) g0 g ...))))
(define-syntax run5 (syntax-rules () ((_ (x) g0 g ...) (run 5 (x) g0 g ...))))
(define-syntax run6 (syntax-rules () ((_ (x) g0 g ...) (run 6 (x) g0 g ...))))
(define-syntax run7 (syntax-rules () ((_ (x) g0 g ...) (run 7 (x) g0 g ...))))
(define-syntax run8 (syntax-rules () ((_ (x) g0 g ...) (run 8 (x) g0 g ...))))
(define-syntax run9 (syntax-rules () ((_ (x) g0 g ...) (run 9 (x) g0 g ...))))
(define-syntax run10 (syntax-rules () ((_ (x) g0 g ...) (run 10 (x) g0 g ...))))

(define-syntax run11 (syntax-rules () ((_ (x) g0 g ...) (run 11 (x) g0 g ...))))
(define-syntax run12 (syntax-rules () ((_ (x) g0 g ...) (run 12 (x) g0 g ...))))
(define-syntax run13 (syntax-rules () ((_ (x) g0 g ...) (run 13 (x) g0 g ...))))
(define-syntax run14 (syntax-rules () ((_ (x) g0 g ...) (run 14 (x) g0 g ...))))
(define-syntax run15 (syntax-rules () ((_ (x) g0 g ...) (run 15 (x) g0 g ...))))
(define-syntax run16 (syntax-rules () ((_ (x) g0 g ...) (run 16 (x) g0 g ...))))
(define-syntax run17 (syntax-rules () ((_ (x) g0 g ...) (run 17 (x) g0 g ...))))
(define-syntax run18 (syntax-rules () ((_ (x) g0 g ...) (run 18 (x) g0 g ...))))
(define-syntax run19 (syntax-rules () ((_ (x) g0 g ...) (run 19 (x) g0 g ...))))
(define-syntax run20 (syntax-rules () ((_ (x) g0 g ...) (run 20 (x) g0 g ...))))

(define-syntax run21 (syntax-rules () ((_ (x) g0 g ...) (run 21 (x) g0 g ...))))
(define-syntax run22 (syntax-rules () ((_ (x) g0 g ...) (run 22 (x) g0 g ...))))
(define-syntax run23 (syntax-rules () ((_ (x) g0 g ...) (run 23 (x) g0 g ...))))
(define-syntax run24 (syntax-rules () ((_ (x) g0 g ...) (run 24 (x) g0 g ...))))
(define-syntax run25 (syntax-rules () ((_ (x) g0 g ...) (run 25 (x) g0 g ...))))
(define-syntax run26 (syntax-rules () ((_ (x) g0 g ...) (run 26 (x) g0 g ...))))
(define-syntax run27 (syntax-rules () ((_ (x) g0 g ...) (run 27 (x) g0 g ...))))
(define-syntax run28 (syntax-rules () ((_ (x) g0 g ...) (run 28 (x) g0 g ...))))
(define-syntax run29 (syntax-rules () ((_ (x) g0 g ...) (run 29 (x) g0 g ...))))
(define-syntax run30 (syntax-rules () ((_ (x) g0 g ...) (run 30 (x) g0 g ...))))

(define-syntax run31 (syntax-rules () ((_ (x) g0 g ...) (run 31 (x) g0 g ...))))
(define-syntax run32 (syntax-rules () ((_ (x) g0 g ...) (run 32 (x) g0 g ...))))
(define-syntax run33 (syntax-rules () ((_ (x) g0 g ...) (run 33 (x) g0 g ...))))
(define-syntax run34 (syntax-rules () ((_ (x) g0 g ...) (run 34 (x) g0 g ...))))
(define-syntax run35 (syntax-rules () ((_ (x) g0 g ...) (run 35 (x) g0 g ...))))
(define-syntax run36 (syntax-rules () ((_ (x) g0 g ...) (run 36 (x) g0 g ...))))
(define-syntax run37 (syntax-rules () ((_ (x) g0 g ...) (run 37 (x) g0 g ...))))
(define-syntax run38 (syntax-rules () ((_ (x) g0 g ...) (run 38 (x) g0 g ...))))
(define-syntax run39 (syntax-rules () ((_ (x) g0 g ...) (run 39 (x) g0 g ...))))
(define-syntax run40 (syntax-rules () ((_ (x) g0 g ...) (run 40 (x) g0 g ...))))

(test-check "testc11.tex-1" 
(run* (q)
  fail)

`())

(test-check "testc11.tex-2"   
(run* (q)
  (== #t q))

`(#t))

(test-check "testc11.tex-3"   
(run* (q) 
  fail
  (== #t q))

`())

    (define g fail)
  

(test-check "testc11.tex-4"   
(run* (q) 
  succeed 
  (== #t q))

(list #t))

(test-check "testc11.tex-5"   
(run* (q) 
  succeed 
  (== #t q))

`(#t))

(test-check "testc11.tex-6"   
(run* (r) 
  succeed
  (== 'corn r))

(list 'corn))

(test-check "testc11.tex-7"   
(run* (r) 
  succeed
  (== 'corn r))

`(corn))

(test-check "testc11.tex-8"   
(run* (r)
  fail
  (== 'corn r))

`())

(test-check "testc11.tex-9"   
(run* (q) 
  succeed 
  (== #f q))

`(#f))

(test-check "testc11.tex-10" 
(run* (x)
  (let ((x #f))
    (== #t x)))

'())

(test-check "testc11.tex-11" 
(run* (q)
  (fresh (x)
    (== #t x)
    (== #t q)))

(list #t))

(run* (q)
  (fresh (x)
    (== #t x)
    (== #t q)))


(test-check "testc11.tex-12" 
(run* (q)
  (fresh (x)
    (== x #t)
    (== #t q)))

(list #t))

(test-check "testc11.tex-13" 
(run* (q)
  (fresh (x)
    (== x #t)
    (== q #t)))

(list #t))

(test-check "testc11.tex-14"   
(run* (x)
  succeed)

(list `_.0))

(test-check "testc11.tex-15"   
(run* (x)
  (let ((x #f))
    (fresh (x)
      (== #t x))))

`(_.0))

(test-check "testc11.tex-16" 
(run* (r)
  (fresh (x y)
    (== (cons x (cons y '())) r)))

(list `(_.0 _.1)))

(test-check "testc11.tex-17" 
(run* (s)
  (fresh (t u)
    (== (cons t (cons u '())) s)))

(list `(_.0 _.1)))

(test-check "testc11.tex-18" 
(run* (r)
  (fresh (x)
    (let ((y x))
      (fresh (x)
        (== (cons y (cons x (cons y '()))) r)))))

(list `(_.0 _.1 _.0)))

(test-check "testc11.tex-19" 
(run* (r)
  (fresh (x)
    (let ((y x))
      (fresh (x)
        (== (cons x (cons y (cons x '()))) r)))))

(list `(_.0 _.1 _.0)))

(test-check "testc11.tex-20" 
(run* (q) 
  (== #f q)
  (== #t q))

`())

(test-check "testc11.tex-21"   
(run* (q) 
  (== #f q)
  (== #f q))

'(#f))

(test-check "testc11.tex-22" 
(run* (q)
  (let ((x q))
    (== #t x)))

(list #t))

(test-check "testc11.tex-23" 
(run* (r)
  (fresh (x)
    (== x r)))

(list `_.0))

(test-check "testc11.tex-24" 
(run* (q)
  (fresh (x)
    (== #t x)
    (== x q)))

(list #t))

(test-check "testc11.tex-25" 
(run* (q)
  (fresh (x)
    (== x q)
    (== #t x)))

(list #t))

(run* (q)
  (fresh (x)
    (== #t x)
    (== x q)))


(test-check "testc11.tex-26" 
(run* (q)
  (fresh (x)
    (== (eq? x q) q)))


    (list #f))
  


(define bit-xoro
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 0 x) (== 1 y) (== 1 r))
      ((== 1 x) (== 0 y) (== 1 r))
      ((== 1 x) (== 1 y) (== 0 r)))))


(test-check "testc20.tex-1" 
(run* (s)
  (fresh (x y)
    (bit-xoro x y 0)
    (== `(,x ,y) s)))  
  

`((0 0)
 (1 1))
)

(test-check "testc20.tex-2" 
(run* (s)
  (fresh (x y)
    (bit-xoro x y 1)
    (== `(,x ,y) s)))


`((0 1)
 (1 0))
)

(test-check "testc20.tex-3" 
(run* (s)
  (fresh (x y r)
    (bit-xoro x y r)
    (== `(,x ,y ,r) s)))


`((0 0 0) 
 (0 1 1)
 (1 0 1)
 (1 1 0))
)

(define bit-ando
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 1 x) (== 0 y) (== 0 r))
      ((== 0 x) (== 1 y) (== 0 r))
      ((== 1 x) (== 1 y) (== 1 r)))))


(test-check "testc20.tex-4" 
(run* (s)
  (fresh (x y)
    (bit-ando x y 1)
    (== `(,x ,y) s)))  
  

`((1 1))
)

(define half-addero
  (lambda (x y r c)
    (fresh ()
      (bit-xoro x y r)
      (bit-ando x y c))))


(test-check "testc20.tex-5" 
(run* (r)
  (half-addero 1 1 r 1))

(list 0))

(test-check "testc20.tex-6" 
(run* (s)
  (fresh (x y r c)
    (half-addero x y r c)
    (== `(,x ,y ,r ,c) s)))


`((0 0 0 0)
 (0 1 1 0)
 (1 0 1 0)
 (1 1 0 1))
  )

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))


(test-check "testc20.tex-7" 
(run* (s)
  (fresh (r c)
    (full-addero 0 1 1 r c)
    (== `(,r ,c) s)))

(list `(0 1)))

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


(test-check "testc20.tex-8" 
(run* (s)
  (fresh (r c)
    (full-addero 1 1 1 r c)
    (== `(,r ,c) s)))

(list `(1 1)))

(test-check "testc20.tex-9" 
(run* (s)
  (fresh (b x y r c)
    (full-addero b x y r c)
    (== `(,b ,x ,y ,r ,c) s)))


`((0 0 0 0 0)
 (1 0 0 1 0)
 (0 1 0 1 0)
 (1 1 0 0 1)
 (0 0 1 1 0)
 (1 0 1 0 1)
 (0 1 1 0 1)
 (1 1 1 1 1))
)


(define build-num
  (lambda (n)
    (cond
      ((zero? n) '())
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2)))))))


(test-check "testc20.tex-10" `(1 0 1)

    (build-num
 
5

    ))
 

(test-check "testc20.tex-11" `(1 1 1)

    (build-num 
 
7

    ))
 
(test-check "nine" (build-num 
9

    )

`(1 0 0 1)

    )

(test-check "six" (build-num 
6

    )

`(0 1 1)

    )

(test-check "nineteen" (build-num 
19

    )

`(1 1 0 0 1)

    )

(test-check "biggie" (build-num 
17290

    )

`(0 1 0 1 0 0 0 1 1 1 0 0 0 0 1)

    )


(test-check "testc20.tex-12" (build-num 0)
`())

(test-check "testc20.tex-13" (build-num 36)
`(0 0 1 0 0 1))

(test-check "testc20.tex-14" (build-num 19)
`(1 1 0 0 1))


(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))    
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))


(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))


(test-check "testc20.tex-15" 
(run* (q)
  (poso '(0 1 1))
  (== #t q))

(list #t))

(test-check "testc20.tex-16" 
(run* (q)
  (poso '(1))
  (== #t q))

(list #t))

(test-check "testc20.tex-17" 
(run* (q)
  (poso '())
  (== #t q))

`())

(test-check "testc20.tex-18" 
(run* (r)
  (poso r))

(list `(_.0 . _.1)))

(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== `(,a ,ad . ,dd) n))))


(test-check "testc20.tex-19" 
(run* (q)
  (>1o '(0 1 1))
  (== #t q))

(list #t))

(test-check "testc20.tex-20" 
(run* (q)
  (>1o '(0 1))
  (== #t q))

`(#t))

(test-check "testc20.tex-21" 
(run* (q)
  (>1o '(1))
  (== #t q))

`())

(test-check "testc20.tex-22" 
(run* (q)
  (>1o '())
  (== #t q))

`())

(test-check "testc20.tex-23" 
(run* (r)
  (>1o r))

(list 
`(_.0 _.1 . _.2)
))


(define addero
  (lambda (d n m r)
    (conde
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

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (full-addero d a b c e)
      (addero e x y z))))


(test-check "testc20.tex-24" 
(run3 (s)
  (fresh (x y r)
    (addero 0 x y r)
    (== `(,x ,y ,r) s)))
  

`((_.0 () _.0)
 (() (_.0 . _.1) (_.0 . _.1))
 ((1) (1) (0 1)))
 )

#;(test-check "testc20.tex-25" 
(run22 (s)
  (fresh (x y r)
    (addero 0 x y r)
    (== `(,x ,y ,r) s)))


`((_.0 () _.0)
 (() (_.0 . _.1) (_.0 . _.1))
 ((1) (1) (0 1))
 ((1) (0 _.0 . _.1) (1 _.0 . _.1))
 ((1) (1 1) (0 0 1))
 ((0 _.0 . _.1) (1) (1 _.0 . _.1))
 ((1) (1 0 _.0 . _.1) (0 1 _.0 . _.1))
 ((0 1) (0 1) (0 0 1))
 ((1) (1 1 1) (0 0 0 1))
 ((1 1) (1) (0 0 1))
 ((1) (1 1 0 _.0 . _.1) (0 0 1 _.0 . _.1))
 ((1 1) (0 1) (1 0 1))
 ((1) (1 1 1 1) (0 0 0 0 1))
 ((1 0 _.0 . _.1) (1) (0 1 _.0 . _.1))
 ((1) (1 1 1 0 _.0 . _.1) (0 0 0 1 _.0 . _.1))
 ((1) (1 1 1 1 1) (0 0 0 0 0 1))
 ((1 1 1) (1) (0 0 0 1))
 ((1) (1 1 1 1 0 _.0 . _.1) (0 0 0 0 1 _.0 . _.1))
 ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))
 ((0 1) (1 1) (1 0 1))
 ((1 1 0 _.0 . _.1) (1) (0 0 1 _.0 . _.1))
 ((1) (1 1 1 1 1 0 _.0 . _.1) (0 0 0 0 0 1 _.0 . _.1)))
)



(test-check "testc20.tex-26" 
(run* (s)
  (gen-addero 1 '(0 1 1) '(1 1) s))

(list `(0 1 0 1)))

(test-check "testc20.tex-27" 
(run* (s)
  (fresh (x y)
    (addero 0 x y '(1 0 1))
    (== `(,x ,y) s)))


`(((1 0 1) ())
 (() (1 0 1))
 ((1) (0 0 1))
 ((0 0 1) (1))
 ((1 1) (0 1))
 ((0 1) (1 1)))
)

(run* (s)
  (fresh (x y)
    (addero 0 x y '(1 0 1))
    (== `(,x ,y) s)))


(define pluso
  (lambda (n m k)
    (addero 0 n m k)))


(run* (s)
  (fresh (x y)
    (pluso x y '(1 0 1))
    (== `(,x ,y) s)))


(test-check "testc20.tex-28" 
(run* (s)
  (fresh (x y)
    (pluso x y '(1 0 1))
    (== `(,x ,y) s)))


`(((1 0 1) ())
 (() (1 0 1))
 ((1) (0 0 1))
 ((0 0 1) (1))
 ((1 1) (0 1))
 ((0 1) (1 1)))
)

(define minuso
  (lambda (n m k)
    (pluso m k n)))


(test-check "testc20.tex-29" 
(run* (q)
  (minuso '(0 0 0 1) '(1 0 1) q))


`((1 1))
)

(test-check "testc20.tex-30" 
(run* (q)
  (minuso '(0 1 1) '(0 1 1) q))


`(())
)

(test-check "testc20.tex-31" 
(run* (q)
  (minuso '(0 1 1) '(0 0 0 1) q))


`()
)


(define *o
  (lambda (n m p)
    (conde
      ((== '() n) (== '() p))
      ((poso n) (== '() m) (== '() p))  
      ((== '(1) n) (poso m) (== m p))   
      ((>1o n) (== '(1) m) (== n p))
      ((fresh (x z)
         (== `(0 . ,x) n) (poso x)
         (== `(0 . ,z) p) (poso z)
         (>1o m)
         (*o x m z)))
      ((fresh (x y)
         (== `(1 . ,x) n) (poso x)
         (== `(0 . ,y) m) (poso y)
         (*o m n p)))
      ((fresh (x y)
         (== `(1 . ,x) n) (poso x)      
         (== `(1 . ,y) m) (poso y)
         (odd-*o x n m p))))))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (pluso `(0 . ,q) m p))))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(define conso
  (lambda (a d res)
    (== (cons a d) res)))

(define nullo
  (lambda (p)
    (== p '())))
(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons d a) p))))
(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))


(define bound-*o
  (lambda (q p n m)
    (conde
      ((nullo q) (pairo p))
      ((fresh (x y z)
         (cdro q x)
         (cdro p y)
         (conde
           ((nullo n)
            (cdro m z)
            (bound-*o x y z '()))
           ((cdro n z) 
            (bound-*o x y z m))))))))


#;(test-check "testc21.tex-1" 
(run34 (t)
  (fresh (x y r)
    (*o x y r)
    (== `(,x ,y ,r) t)))


`((() _.0 ())
 ((_.0 . _.1) () ())
 ((1) (_.0 . _.1) (_.0 . _.1))
 ((_.0 _.1 . _.2) (1) (_.0 _.1 . _.2))
 ((0 1) (_.0 _.1 . _.2) (0 _.0 _.1 . _.2))
 ((0 0 1) (_.0 _.1 . _.2) (0 0 _.0 _.1 . _.2))
 ((1 _.0 . _.1) (0 1) (0 1 _.0 . _.1))
 ((0 0 0 1) (_.0 _.1 . _.2) (0 0 0 _.0 _.1 . _.2))
 ((1 _.0 . _.1) (0 0 1) (0 0 1 _.0 . _.1))
 ((0 1 _.0 . _.1) (0 1) (0 0 1 _.0 . _.1))
 ((0 0 0 0 1) (_.0 _.1 . _.2) (0 0 0 0 _.0 _.1 . _.2))
 ((1 _.0 . _.1) (0 0 0 1) (0 0 0 1 _.0 . _.1))
 ((0 1 _.0 . _.1) (0 0 1) (0 0 0 1 _.0 . _.1))
 ((0 0 1 _.0 . _.1) (0 1) (0 0 0 1 _.0 . _.1))
 ((1 1) (1 1) (1 0 0 1))
 ((0 0 0 0 0 1) (_.0 _.1 . _.2) (0 0 0 0 0 _.0 _.1 . _.2))
 ((1 _.0 . _.1) (0 0 0 0 1) (0 0 0 0 1 _.0 . _.1))
 ((0 1 _.0 . _.1) (0 0 0 1) (0 0 0 0 1 _.0 . _.1))
 ((0 0 1 _.0 . _.1) (0 0 1) (0 0 0 0 1 _.0 . _.1))
 ((0 0 0 1 _.0 . _.1) (0 1) (0 0 0 0 1 _.0 . _.1))
 ((1 1) (1 0 1) (1 1 1 1))
 ((0 1 1) (1 1) (0 1 0 0 1))
 ((1 1) (1 1 1) (1 0 1 0 1))
 ((1 1) (0 1 1) (0 1 0 0 1))
 ((0 0 0 0 0 0 1) (_.0 _.1 . _.2) (0 0 0 0 0 0 _.0 _.1 . _.2))
 ((1 _.0 . _.1) (0 0 0 0 0 1) (0 0 0 0 0 1 _.0 . _.1))
 ((0 1 _.0 . _.1) (0 0 0 0 1) (0 0 0 0 0 1 _.0 . _.1))
 ((0 0 1 _.0 . _.1) (0 0 0 1) (0 0 0 0 0 1 _.0 . _.1))
 ((0 0 0 1 _.0 . _.1) (0 0 1) (0 0 0 0 0 1 _.0 . _.1))
 ((1 0 1) (1 1) (1 1 1 1))
 ((0 0 0 0 1 _.0 . _.1) (0 1) (0 0 0 0 0 1 _.0 . _.1))
 ((0 1 1) (1 0 1) (0 1 1 1 1))
 ((0 0 1 1) (1 1) (0 0 1 0 0 1))
 ((1 1) (1 0 0 1) (1 1 0 1 1)))
)

#;(test-check "testc21.tex-2" 
(run* (p)
  (*o '(0 1) '(0 0 1) p))  

(list `(0 0 0 1)))



(define bound-*o
  (lambda (q p n m)
    succeed))


#;(test-check "testc21.tex-3" 
(run1 (t)
  (fresh (n m)
    (*o n m '(1))
    (== `(,n ,m) t)))

(list `((1) (1))))
(define e (make-engine (lambda () 
(run2 (t)
  (fresh (n m)
    (*o n m '(1))
    (== `(,n ,m) t)))
)))
(printf "Testing testc21.tex-4  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc21.tex-4 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))



(define bound-*o
  (lambda (q p n m)
    (conde
      ((nullo q) (pairo p))
      ((fresh (x y z)
         (cdro q x)
         (cdro p y)
         (conde
           ((nullo n)
            (cdro m z)
            (bound-*o x y z '()))
           ((cdro n z) 
            (bound-*o x y z m))))))))



#;(test-check "testc21.tex-5" 
(run2 (t)
  (fresh (n m)
    (*o n m '(1))
    (== `(,n ,m) t)))

`(((1) (1))))

#;(test-check "testc21.tex-6" 
(run* (p)
  (*o '(1 1 1) '(1 1 1 1 1 1) p))

(list `(1 0 0 1 1 1 0 1 1)))

(define =lo
  (lambda (n m)
    (conde
      ((== '() n) (== '() m))
      ((== '(1) n) (== '(1) m))
      ((fresh (a x b y)
         (== `(,a . ,x) n) (poso x)
         (== `(,b . ,y) m) (poso y)
         (=lo x y))))))


#;(test-check "testc21.tex-7" 
(run* (t)
  (fresh (w x y)
    (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1))
    (== `(,w ,x ,y) t)))

(list `(_.0 _.1 (_.2 1))))

#;(test-check "testc21.tex-8" 
(run* (b)
  (=lo '(1) `(,b)))

(list 1))

#;(test-check "testc21.tex-9" 
(run* (n)
  (=lo `(1 0 1 . ,n) '(0 1 1 0 1)))

(list 
`(_.0 1)
))

#;(test-check "testc21.tex-10" 
(run5 (t)
  (fresh (y z)
    (=lo `(1 . ,y) `(1 . ,z))
    (== `(,y ,z) t)))


`((() ())
 ((1) (1))
 ((_.0 1) (_.1 1))
 ((_.0 _.1 1) (_.2 _.3 1))
 ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1)))
)

#;(test-check "testc21.tex-11" 
(run5 (t)
  (fresh (y z)
    (=lo `(1 . ,y) `(0 . ,z))
    (== `(,y ,z) t)))


`(((1) (1))
 ((_.0 1) (_.1 1))
 ((_.0 _.1 1) (_.2 _.3 1))
 ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
 ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1)))
)

#;(test-check "testc21.tex-12" 
(run5 (t)
  (fresh (y z)
    (=lo `(1 . ,y) `(0 1 1 0 1 . ,z))
    (== `(,y ,z) t)))


`(((_.0 _.1 _.2 1) ())
 ((_.0 _.1 _.2 _.3 1) (1))
 ((_.0 _.1 _.2 _.3 _.4 1) (_.5 1))
 ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 1))
 ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 1) (_.7 _.8 _.9 1)))
)

(define <lo
  (lambda (n m)
    (conde
      ((== '() n) (poso m))
      ((== '(1) n) (>1o m))
      ((fresh (a x b y)
         (== `(,a . ,x) n) (poso x)
         (== `(,b . ,y) m) (poso y)
         (<lo x y))))))


#;(test-check "testc21.tex-13" 
(run8 (t)
  (fresh (y z)
    (<lo `(1 . ,y) `(0 1 1 0 1 . ,z))
    (== `(,y ,z) t)))


`((() _.0)
 ((1) _.0)
 ((_.0 1) _.1)
 ((_.0 _.1 1) _.2)
 ((_.0 _.1 _.2 1) (_.3 . _.4))
 ((_.0 _.1 _.2 _.3 1) (_.4 _.5 . _.6))
 ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 . _.8))
 ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 . _.10)))
)
(define e (make-engine (lambda () 
(run1 (n)
  (<lo n n))
)))
(printf "Testing testc21.tex-14  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc21.tex-14 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))


(define <=lo
  (lambda (n m)
    (conde
      ((=lo n m))
      ((<lo n m)))))


#;(test-check "testc21.tex-15" 
(run8 (t)
  (fresh (n m)
    (<=lo n m)
    (== `(,n ,m) t)))


`((() ())
 ((1) (1))
 (() (_.0 . _.1))
 ((1) (_.0 _.1 . _.2))
 ((_.0 1) (_.1 1))
 ((_.0 1) (_.1 _.2 _.3 . _.4))
 ((_.0 _.1 1) (_.2 _.3 1))
 ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1)))
)

#;(test-check "testc21.tex-16" 
(run1 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n '(0 1) m)
    (== `(,n ,m) t)))

(list `(() ())))

#;(test-check "testc21.tex-17" 
(run10 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n '(0 1) m)
    (== `(,n ,m) t)))


`((() ())
 ((1) (0 1))
 ((0 1) (0 0 1))
 ((1 1) (0 1 1))
 ((1 _.0 1) (0 1 _.0 1))
 ((0 0 1) (0 0 0 1))
 ((0 1 1) (0 0 1 1))
 ((1 _.0 _.1 1) (0 1 _.0 _.1 1))
 ((0 1 _.0 1) (0 0 1 _.0 1))
 ((0 0 0 1) (0 0 0 0 1)))
)

#;(test-check "testc21.tex-18" 
(run15 (t)
  (fresh (n m)
    (<=lo n m)
    (== `(,n ,m) t)))


`((() ())
 ((1) (1))
 (() (_.0 . _.1))
 ((1) (_.0 _.1 . _.2))
 ((_.0 1) (_.1 1))
 ((_.0 1) (_.1 _.2 _.3 . _.4))
 ((_.0 _.1 1) (_.2 _.3 1))
 ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
 ((_.0 _.1 1) (_.2 _.3 _.4 _.5 . _.6))
 ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1))
 ((_.0 _.1 _.2 1) (_.3 _.4 _.5 _.6 _.7 . _.8))
 ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 _.8 _.9 1))
 ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 _.8 _.9 . _.10))
 ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 _.10 _.11 1))
 ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 . _.12)))
)

(define <o
  (lambda (n m)
    (conde
      ((<lo n m))
      ((=lo n m)
       (fresh (x)
         (poso x)
         (pluso n x m))))))


(define <=o
  (lambda (n m)
    (conde
      ((== n m))
      ((<o n m)))))


#;(test-check "testc21.tex-19" 
(run* (q)
  (<o '(1 0 1) '(1 1 1))
  (== #t q))

(list #t))

#;(test-check "testc21.tex-20" 
(run* (q)
  (<o '(1 1 1) '(1 0 1))
  (== #t q))

`())

#;(test-check "testc21.tex-21" 
(run* (q)
  (<o '(1 0 1) '(1 0 1))
  (== #t q))

`())

#;(test-check "lessthanequalo-1"
  (run* (q)
    (<=o '(1 0 1) '(1 0 1))
    (== #t q))

`(#t))

#;(test-check "testc21.tex-22" 
(run6 (n)
  (<o n `(1 0 1)))


`(() (1) (_.0 1) (0 0 1))
)

#;(test-check "testc21.tex-23" 
(run6 (m)
  (<o `(1 0 1) m))


`((_.0 _.1 _.2 _.3 . _.4) (0 1 1) (1 1 1))
)
(define e (make-engine (lambda () 
(run* (n)
  (<o n n))
)))
(printf "Testing testc21.tex-24  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc21.tex-24 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))



(define /o
  (lambda (n m q r)
    (conde
      ((== r n) (== '() q) (<o n m))
      ((== '(1) q) (=lo n m) (pluso r m n)
       (<o r m))
      ((<lo m n)                        
       (<o r m)                        
       (poso q)                 
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (splito n r nl nh)
         (splito q r ql qh)
         (conde
           ((== '() nh)
            (== '() qh)
            (minuso nl r qlm)
            (*o ql m qlm))
           ((poso nh)
            (*o ql m qlm)
            (pluso qlm r qlmr)
            (minuso qlmr nl rr)
            (splito rr r '() rh)
            (/o nh m qh rh))))))))

(define splito
  (lambda (n r l h)
    (conde
      ((== '() n) (== '() h) (== '() l))
      ((fresh (b n^)
         (== `(0 ,b . ,n^) n)
         (== '() r)
         (== `(,b . ,n^) h)
         (== '() l)))
      ((fresh (n^)
         (==  `(1 . ,n^) n)
         (== '() r)
         (== n^ h)
         (== '(1) l)))
      ((fresh (b n^ a r^)
         (== `(0 ,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== '() l)
         (splito `(,b . ,n^) r^ '() h)))
      ((fresh (n^ a r^)
         (== `(1 . ,n^) n)
         (== `(,a . ,r^) r)
         (== '(1) l)
         (splito n^ r^ '() h)))
      ((fresh (b n^ a r^ l^)
         (== `(,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== `(,b . ,l^) l)
         (poso l^)
         (splito n^ r^ l^ h))))))


#;(test-check "testc21.tex-25" 
(run6 (t)
  (fresh (n m q r)
    (/o n m q r)
    (== `(,n ,m ,q ,r) t)))


`((() (_.0 . _.1) () ())
 ((1) (_.0 _.1 . _.2) () (1))
 ((_.0 1) (_.1 _.2 _.3 . _.4) () (_.0 1))
 ((_.0 _.1 1) (_.2 _.3 _.4 _.5 . _.6) () (_.0 _.1 1))
 ((_.0 _.1 _.2 1) (_.3 _.4 _.5 _.6 _.7 . _.8) () (_.0 _.1 _.2 1))
 ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 _.8 _.9 . _.10) () (_.0 _.1 _.2 _.3 1)))
)





(define /o
  (lambda (n m q r)
    (conde
      ((== '() q) (== n r) (<o n m))
      ((== '(1) q) (== '() r) (== n m)
       (<o r m))      
      ((<o m n) (<o r m)
       (fresh (mq)
         (<=lo mq n)
         (*o m q mq)
         (pluso mq r n))))))
  


  (define /otest1
    (lambda ()


(run3 (t)
  (fresh (y z)
    (/o `(1 0 . ,y) '(0 1) z '())
    (== `(,y ,z) t)))


  ))
(define e (make-engine /otest1))
(printf "Testing testc23.tex-/otest1  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc23.tex-/otest1 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))



(define /o
  (lambda (n m q r)
    (conde
      ((== r n) (== '() q) (<o n m))
      ((== '(1) q) (=lo n m) (pluso r m n)
       (<o r m))
      ((<lo m n)                        
       (<o r m)                        
       (poso q)                 
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (splito n r nl nh)
         (splito q r ql qh)
         (conde
           ((== '() nh)
            (== '() qh)
            (minuso nl r qlm)
            (*o ql m qlm))
           ((poso nh)
            (*o ql m qlm)
            (pluso qlm r qlmr)
            (minuso qlmr nl rr)
            (splito rr r '() rh)
            (/o nh m qh rh))))))))

(define splito
  (lambda (n r l h)
    (conde
      ((== '() n) (== '() h) (== '() l))
      ((fresh (b n^)
         (== `(0 ,b . ,n^) n)
         (== '() r)
         (== `(,b . ,n^) h)
         (== '() l)))
      ((fresh (n^)
         (==  `(1 . ,n^) n)
         (== '() r)
         (== n^ h)
         (== '(1) l)))
      ((fresh (b n^ a r^)
         (== `(0 ,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== '() l)
         (splito `(,b . ,n^) r^ '() h)))
      ((fresh (n^ a r^)
         (== `(1 . ,n^) n)
         (== `(,a . ,r^) r)
         (== '(1) l)
         (splito n^ r^ '() h)))
      ((fresh (b n^ a r^ l^)
         (== `(,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== `(,b . ,l^) l)
         (poso l^)
         (splito n^ r^ l^ h))))))


(define logo
 (lambda (n b q r)
   (conde
     ((== '(1) n) (poso b) (== '() q) (== '() r))
     ((== '() q) (<o n b) (pluso r '(1) n))
     ((== '(1) q) (>1o b) (=lo n b) (pluso r b n))
     ((== '(1) b) (poso q) (pluso r '(1) n))
     ((== '() b) (poso q) (== r n))
     ((== '(0 1) b)
      (fresh (a ad dd)
        (poso dd)
        (== `(,a ,ad . ,dd) n)
        (exp2 n '() q)
        (fresh (s)
          (splito n dd r s))))
     ((fresh (a ad add ddd)
        (conde
          ((== '(1 1) b))
          ((== `(,a ,ad ,add . ,ddd) b))))
      (<lo b n)
      (fresh (bw1 bw nw nw1 ql1 ql s)
        (exp2 b '() bw1)
        (pluso bw1 '(1) bw)
        (<lo q n)
        (fresh (q1 bwq1)
          (pluso q '(1) q1)
          (*o bw q1 bwq1)
          (<o nw1 bwq1))
          (exp2 n '() nw1)
          (pluso nw1 '(1) nw)
          (/o nw bw ql1 s)
          (pluso ql '(1) ql1)
          (<=lo ql q)
          (fresh (bql qh s qdh qd)
            (repeated-mul b ql bql)
            (/o nw bw1 qh s)
            (pluso ql qdh qh)
            (pluso ql qd q)
            (<=o qd qdh)
            (fresh (bqd bq1 bq)
              (repeated-mul b qd bqd)
              (*o bql bqd bq)
              (*o b bq bq1)
              (pluso bq r n)
              (<o n bq1))))))))


(define exp2
  (lambda (n b q)
    (conde
      ((== '(1) n) (== '() q))
      ((>1o n) (== '(1) q)
       (fresh (s)
         (splito n b s '(1))))
      ((fresh (q1 b2)
         (== `(0 . ,q1) q)
         (poso q1)
         (<lo b n)
         (appendo b `(1 . ,b) b2)
         (exp2 n b2 q1)))
      ((fresh (q1 nh b2 s)
         (== `(1 . ,q1) q)
         (poso q1)
         (poso nh)
         (splito n b s nh)
         (appendo b `(1 . ,b) b2)
         (exp2 nh b2 q1))))))


(define repeated-mul
  (lambda (n q nq)
    (conde
      ((poso n) (== '() q) (== '(1) nq))
      ((== '(1) q) (== n nq))
      ((>1o q)
       (fresh (q1 nq1)
         (pluso q1 '(1) q)
         (repeated-mul n q1 nq1)
         (*o nq1 n nq))))))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (caro l a)
         (cdro l d)   
         (appendo d s res)
         (conso a res out))))))

(test-check "testc21.tex-26" 
(run* (r) 
  (logo '(0 1 1 1) '(0 1) '(1 1) r))

(list `(0 1 1)))


 '(printf "This next test takes several minutes to run!\n")

(time
(test-check "testc21.tex-27" 
(run9 (s)
  (fresh (b q r)
    (logo '(0 0 1 0 0 0 1) b q r)
    (>1o q)
    (== `(,b ,q ,r) s)))


`((() (_.0 _.1 . _.2) (0 0 1 0 0 0 1))
 ((1) (_.0  _.1 . _.2) (1 1 0 0 0 0 1))
 ((0 1) (0 1 1) (0 0 1))
 ((1 1) (1 1) (1 0 0 1 0 1))
 ((0 0 1) (1 1) (0 0 1))
 ((0 0 0 1) (0 1) (0 0 1))
 ((1 0 1) (0 1) (1 1 0 1 0 1))
 ((0 1 1) (0 1) (0 0 0 0 0 1))
 ((1 1 1) (0 1) (1 1 0 0 1)))
))

(define expo
  (lambda (b q n)
    (logo n b q '())))


'(test-check "testc21.tex-28" 
(run* (t)
  (expo '(1 1) '(1 0 1) t))

(list `(1 1 0 0 1 1 1 1)))

#|
 (define u (var 'u))
 
 (define v (var 'v))
 
 (define w (var 'w))
 

 (define x (var 'x))
 
 (define y (var 'y))
 
 (define z (var 'z))
 

(test-check "testc22.tex-1"   
(rhs `(,z . b))

'b)

(test-check "testc22.tex-2"   
(rhs `(,z . ,w))

w)

(test-check "testc22.tex-3" 
(rhs `(,z . (,x e ,y)))

`(,x e ,y))


(test-check "testc22.tex-4" 
(walk z `((,z . a) (,x . ,w) (,y . ,z)))

'a)

(test-check "testc22.tex-5"   
(walk y `((,z . a) (,x . ,w) (,y . ,z)))

'a)

(test-check "testc22.tex-6"   
(walk x `((,z . a) (,x . ,w) (,y . ,z)))

w)

(test-check "testc22.tex-7"   
(walk w `((,z . a) (,x . ,w) (,y . ,z)))

w)

(test-check "testc22.tex-8"   
(walk u `((,x . b) (,w . (,x e ,x)) (,u . ,w)))

`(,x e ,x))


(test-check "testc22.tex-9" 
(walk y (ext-s x 'e `((,z . ,x) (,y . ,z))))

'e)

(test-check "testc22.tex-10"                                                    
(walk y `((,x . e)))                                                            

y)

(test-check "testc22.tex-11"   
(walk x `((,y . ,z) (,x . ,y)))

z)

(test-check "testc22.tex-12"   
(walk x (ext-s y z `((,x . ,y))))

z)

(test-check "testc22.tex-13" 
(walk x (ext-s z 'b `((,y . ,z) (,x . ,y))))

'b)

(test-check "testc22.tex-14" 
(walk x (ext-s z w `((,y . ,z) (,x . ,y))))

w)


(test-check "testc22.tex-15" 
(occurs-check z u 
  `((,x . (a ,y)) (,w . (,x e ,x)) (,u . ,w) (,y . (,z))))

#t)



(test-check "testc22.tex-16"   
(walk* x
  `((,y . (a ,z c)) (,x . ,y) (,z . a)))

`(a a c))

(test-check "testc22.tex-17" 
(walk* x
  `((,y . (,z ,w c)) (,x . ,y) (,z . a)))

`(a ,w c))

(test-check "testc22.tex-18" 
(walk* y
  `((,y . (,w ,z c)) (,v . b) (,x . ,v) (,z . ,x)))

`(,w b c))



(test-check "testc22.tex-19" 
(run* (q)
  (== #f q)
  (project (q)
    (== (not (not q)) q)))

'(#f))



(test-check "testc22.tex-20" 
(let ((r (walk* `(,x ,y ,z) empty-s)))
  (walk* r (reify-s r empty-s)))

`(_.0 _.1 _.2))

(test-check "testc22.tex-21" 
(let ((r `(,u (,v (,w ,x) ,y) ,x)))
  (walk* r (reify-s r empty-s)))

`(_.0 (_.1 (_.2 _.3) _.4) _.3))

(test-check "testc22.tex-22" 
(let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
  (let ((r (walk* x s)))
    (walk* r (reify-s r empty-s))))

`(a _.0 c _.0))

(test-check "testc22.tex-23" 
(let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . ,u))))
  (let ((r (walk* x s)))
    (walk* r (reify-s r empty-s))))

`(_.0 _.1 c _.1))


(test-check "testc22.tex-24" 
(let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
  (reify x s))

`(a _.0 c _.0))
(define e (make-engine (lambda ()   
(run1 (x) 
  (== `(,x) x))
)))
(printf "Testing testc22.tex-25  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc22.tex-25 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))

(test-check "testc22.tex-28"   
(run1 (x) 
  (== `(,x) x))

`())
(define e (make-engine (lambda () 
(run1 (x)
  (fresh (y z)
    (== x z)
    (== `(a b ,z) y)
    (== x y)))
)))
(printf "Testing testc22.tex-29  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc22.tex-29 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))


(test-check "testc22.tex-30" 
(run1 (x)
  (fresh (y z)
    (== x z)
    (== `(a b ,z) y)
    (== x y)))
`())
(define e (make-engine (lambda ()   
(run1 (x)
  (== `(,x) x))
)))
(printf "Testing testc22.tex-31  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc22.tex-31 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))


    (test-check "testc23.tex-fail1" (run* (q)
  

(conda 
  (fail succeed) 
  (fail)) 


    ) '())
  

    (test-check "testc23.tex-succeed1" (not (null? (run* (q)
  

(conda
  (fail succeed)
  (succeed))


    ))) #t)
  

    (test-check "testc23.tex-succeed1" (not (null? (run* (q)
  

(conda
  (succeed fail)
  (succeed))


    ))) #f)
  

    (test-check "testc23.tex-succeed2" (not (null? (run* (q)
  

(conda
  (succeed succeed)
  (fail))


    ))) #t)
  

(test-check "testc23.tex-1" 
(run* (x)
  (conda
    ((== 'olive x) succeed)
    ((== 'oil x) succeed)))

`(olive))

(test-check "testc23.tex-2" 
(run* (x)
  (conda
    ((== 'virgin x) fail)
    ((== 'olive x) succeed)
    ((== 'oil x) succeed)))

`())

(test-check "testc23.tex-3" 
(run* (q)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      ((== 'split x) (== x y))
      (succeed)))
  (== #t q))

`())

(test-check "testc23.tex-4" 
(run* (q)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      ((== x y) (== 'split x))
      (succeed)))
  (== #t q))

(list #t))
                                                 
(define notpastao
  (lambda (x)                                                                   
    (conda                                                                      
      ((== 'pasta x) fail)                             
      (succeed))))                                                         


(test-check "testc23.tex-5"     
(run* (x) 
  (conda
    ((notpastao x) fail)
    ((== 'spaghetti x))))

'(spaghetti))

(test-check "testc23.tex-6" 
(run* (x)                                                                       
  (== 'spaghetti x)  
  (conda
    ((notpastao x) fail)
    ((== 'spaghetti x))))

'())
(define e (make-engine (lambda () 
(run* (q)
  (conda
    (always succeed)
    (fail))
  (== #t q))
)))
(printf "Testing testc23.tex-7  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc23.tex-7 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))


(test-check "testc23.tex-8" 
(run* (q)
  (condu
    (always succeed)
    (fail))
  (== #t q))

`(#t))
(define e (make-engine (lambda () 
(run* (q)
  (condu
    (succeed always)
    (fail))
  (== #t q))
)))
(printf "Testing testc23.tex-9  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc23.tex-9 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))

(define e (make-engine (lambda ()   
(run1 (q)
  (conda
    (always succeed)
    (fail)) 
  fail
  (== #t q))
)))
(printf "Testing testc23.tex-10  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc23.tex-10 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))


(test-check "testc23.tex-11"   
(run1 (q)
  (condu
    (always succeed)
    (fail)) 
  fail
  (== #t q))

`())

(define onceo
  (lambda (g)
    (condu
      (g succeed))))


(test-check "testc23.tex-12" 
(run* (x)
  (onceo (teacupo x)))

`(tea))

(test-check "testc23.tex-13" 
(run1 (q)
  (onceo (salo never))
  fail)

`())

(test-check "testc23.tex-14"   
(run* (r)
  (conde
    ((teacupo r) succeed)
    ((== #f r) succeed)))

`(#f tea cup))

(test-check "testc23.tex-15"   
(run* (r)
  (conda
    ((teacupo r) succeed)
    ((== #f r) succeed)))

`(tea cup))

(test-check "testc23.tex-16" 
(run* (r)
  (== #f r)
  (conda
    ((teacupo r) succeed)
    ((== #f r) succeed)))

`(#f))

(test-check "testc23.tex-17"   
(run* (r)
  (== #f r)
  (condu
    ((teacupo r) succeed)
    ((== #f r) succeed)))

`(#f))

(define bumpo
  (lambda (n x)
    (conde
      ((== n x) succeed)
      ((fresh (m)
         (minuso n '(1) m)
         (bumpo m x))))))


(test-check "testc23.tex-18" 
(run* (x)
  (bumpo '(1 1 1) x))


`((1 1 1)
 (0 1 1)
 (1 0 1)
 (0 0 1)
 (1 1)
 (0 1)
 (1)
 ())
)

(define gen&testo
  (lambda (op i j k)
    (onceo
      (fresh (x y z)
        (op x y z)
        (== i x)
        (== j y)
        (== k z)))))


(test-check "testc23.tex-19" 
(run* (q)
  (gen&testo pluso '(0 0 1) '(1 1) '(1 1 1))
  (== #t q))

(list 
#t
))
(define e (make-engine (lambda () 
(run1 (q)
  (gen&testo pluso '(0 0 1) '(1 1) '(0 1 1)))
)))
(printf "Testing testc23.tex-20  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc23.tex-20 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))

(define e (make-engine (lambda () 
(run1 (q)
  (gen&testo pluso '(0 0 1) '(1 1) '(0 1 1)))
)))
(printf "Testing testc23.tex-21  (engine with ~s ticks fuel)\n" max-ticks)
(e max-ticks
(lambda (t v) (error 'testc23.tex-21 "infinite loop returned ~s after ~s ticks" v (- max-ticks t)))
(lambda (e^) (void)))


(define enumerateo
  (lambda (op r n)
    (fresh (i j k)
      (bumpo n i)
      (bumpo n j)
      (op i j k)
      (gen&testo op i j k)
      (== `(,i ,j ,k) r))))


(test-check "testc23.tex-22" 
(run* (s)
  (enumerateo pluso s '(1 1)))


`(((1 1) (1 1) (0 1 1))
 ((1 1) (0 1) (1 0 1))
 ((1 1) () (1 1))
 ((0 1) (1 1) (1 0 1))
 ((1 1) (1) (0 0 1))
 ((1) (1 1) (0 0 1))
 ((0 1) (0 1) (0 0 1))
 (() (1 1) (1 1))
 ((0 1) () (0 1))
 ((0 1) (1) (1 1))
 ((1) (0 1) (1 1))
 ((1) (1) (0 1))
 ((1) () (1))
 (() (0 1) (0 1))
 (() (1) (1))
 (() () ()))
)

(run* (s)
  (enumerateo pluso s '(1 1)))


'(test-check "testc23.tex-23" 
(run1 (s)
  (enumerateo pluso s '(1 1 1)))


`(((1 1 1) (1 1 1) (0 1 1 1)))
)







;;;  Will's toys:

(define proof-that-fresh-needs-an-inc
  (fresh ()
    proof-that-fresh-needs-an-inc))

(test-check 'proof-that-run-needs-an-inc
  (run 1 (q)
    (conde
      (proof-that-fresh-needs-an-inc)
      (succeed)))
  '(_.0))

(define proof-that-fresh-needs-an-inc-with-conda
  (conda
    (proof-that-fresh-needs-an-inc)))

(test-check 'proof-that-run-needs-an-inc-with-conde-and-conda
  (run 1 (q)
    (conde
      (proof-that-fresh-needs-an-inc)
      (succeed)))
  '(_.0))

(define proof-that-fresh-needs-an-inc-with-conda
  (fresh ()
    (conda
      (proof-that-fresh-needs-an-inc succeed))))

(test-check 'proof-that-run-needs-an-inc-with-conde
  (run 1 (q)
    (conde
      (proof-that-fresh-needs-an-inc succeed)
      (succeed)))
  '(_.0))

(test-check 'why-conde-must-also-have-an-inc
  ((make-engine 
     (lambda () 
       (run 5 (q) 
         (letrec ((f (fresh () 
                       (conde 
                         (f (conde 
                              (f) 
                              (succeed))) 
                         (succeed))))) 
           f)))) 
   100000 
   (lambda (x y) y) 
   list)
  '(_.0 _.0 _.0 _.0 _.0))


;;;  Define 'test-check' once again, for the end-user.
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
|#
