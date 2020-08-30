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
         (== v `(code ,t0))))
      ((fresh (t1 t2 c1 c2) ; list
         (== t `(list ,t1 ,t2))
         (== v `(code ,(list c1 c2)))
         (ev e t1 `(code ,c1))
         (ev e t2 `(code ,c2))))
      (
       (symbolo t)
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

(define normalize-var-name
  (lambda (n)
    n))

(define var-unbound?
  (lambda (x)
    (and (symbol? x)
         (let ((s (symbol->string x)))
           (and (> (string-length s) 2)
                (char=? #\_ (string-ref s 0))
                (char=? #\. (string-ref s 1)))))))

(define normalize
  (lambda (t)
    (if (list? t)
        (if (and (not (null? t)) (eq? 'vr (car t)) (null? (cddr t)))
            (normalize-var-name (cadr t))
            (map normalize t))
        (begin
          (assert (not (eq? 'vr t)))
          (if (var-unbound? t) `(quote ,t) t)))))

(define ok
  (lambda (r)
    (assert (not (null? r)))
    (map pretty-print r)
    (length r)))

(define quine
  '((lambda ((vr z)) (list (vr z) (list (quote quote) (vr z))))
    (quote (lambda ((vr z)) (list (vr z) (list (quote quote) (vr z)))))))

(run* (q)
      (ev '()
          quine
          `(code ,quine)))

(time
   (run 50 (q)
     (ev '()
         q
         `(code ,q))))

(time
   (run 2 (q)
     (fresh (a b)
       (== q `(,a ,b))
       (ev '() a `(code ,b))
       (ev '() b `(code ,a)))))
