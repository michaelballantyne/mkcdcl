(define vlookup
  (lambda (e x v)
    (conde
      ((fresh (er)
         (== e (cons `(,x ,v) er))))
      ((fresh (y vy er)
         (== e (cons `(,y ,vy) er))
         (=/= x y)
         (vlookup er x v))))))

(define ev
  (lambda (e t v)
    (conde
      ((fresh (t0) ; quote
         (== t `(quote ,t0))
         (absento 'clo t0)
         (== v t0)))
      ((fresh (t1 t2 c1 c2) ; list
         (== t `(list ,t1 ,t2))
         (== v (list c1 c2))
         (absento 'clo t1)
         (absento 'clo t2)
         (ev e t1 c1)
         (ev e t2 c2)))
      ((symbolo t)
       (vlookup e t v))
      ((fresh (t1 t2 e0 x0 t0 v2) ; app
         (== t `(,t1 ,t2))
         (symbolo x0)
         (ev e t1 `(clo ,e0 ,x0 ,t0))
         (ev e t2 v2)
         (ev (cons `(,x0 ,v2) e0) t0 v)))
      ((fresh (x t0) ; lam
         (== t `(lambda (,x) ,t0))
         (symbolo x)
         (== v `(clo ,e ,x ,t0))))
      )))


(define quine
  '((lambda (x) (list x (list (quote quote) x)))
    (quote (lambda (x) (list x (list (quote quote) x))))))

(always-wrap-reified? #t)

(run* (q)
    (ev '()
        quine
        quine))

(time
  (run 50 (q)
     (ev '()
         q
         q)))

(time
(run 2 (q)
     (fresh (a b)
       (== q `(,a ,b))
       (ev '() a b)
       (ev '() b a))))
