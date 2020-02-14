(define nat
  (lambda (o)
    (conde
      ((== o 'z))
      ((fresh (n)
         (== o `(s ,n))
         (nat n))))))

(define tm
  (lambda (o)
    (conde
      ((fresh (n)
         (== o `(vr ,n))
         (nat n)))
      ((== o 'quote))
      ((fresh (n t)
         (== o `(lambda ((vr ,n)) ,t))
         (nat n)
         (tm t)))
      ((fresh (t1 t2)
         (== o `(,t1 ,t2))
         (tm t1)
         (tm t2)))
      ((fresh (t1 t2)
         (== o `(list ,t1 ,t2))
         (tm t1)
         (tm t2))))))


(define vl
  (lambda (o)
    (conde
      ((fresh (e n t)
         (== o `(clo ,e ,n ,t))
         (venv e)
         (nat n)
         (tm t)))
      ((fresh (t)
         (== o `(code ,t))
         (tm t))))))


(define venv
  (lambda (o)
    (conde
      ((== o '()))
      ((fresh (n v e)
         (== o (cons `(,n ,v) e))
         (nat n)
         (vl v)
         (venv e))))))

(define neq
  (lambda (n1 n2)
    (conde
      ((== n1 'z)
       (fresh (n2-1)
         (== n2 `(s ,n2-1))))
      ((== n2 'z)
       (fresh (n1-1)
         (== n1 `(s ,n1-1))))
      ((fresh (n1-1 n2-1)
         (== n1 `(s ,n1-1))
         (== n2 `(s ,n2-1))
         (neq n1-1 n2-1))))))


(define vlookup
  (lambda (e x v)
    (conde
      ((fresh (er)
         (== e (cons `(,x ,v) er))))
      ((fresh (y vy er)
         (== e (cons `(,y ,vy) er))
         (neq x y)
         (vlookup er x v))))))

(define ev
  (lambda (e t v)
    (conde
      ((fresh (x)
         (== t `(vr ,x))
         (vlookup e x v)))
      ((fresh (x t0)
         (== t `(lambda ((vr ,x)) ,t0))
         (== v `(clo ,e ,x ,t0))))
      ((fresh (t0)
         (== t `(quote ,t0))
         (== v `(code ,t0))))
      ((fresh (t1 t2 e0 x0 t0 v2)
         (== t `(,t1 ,t2))
         (ev e t1 `(clo ,e0 ,x0 ,t0))
         (ev e t2 v2)
         (ev (cons `(,x0 ,v2) e0) t0 v)))
      ((fresh (t1 t2 c1 c2)
         (== t `(list ,t1 ,t2))
         (ev e t1 `(code ,c1))
         (ev e t2 `(code ,c2))
         (== v `(code ,(list c1 c2))))))))

(define normalize-var-name
  (lambda (n)
    (if (and (list? n) (eq? 's (car n)) (null? (cddr n)))
        (string->symbol (string-append
                         (symbol->string (car n))
                         (symbol->string (normalize-var-name (cadr n)))))
        (begin
          (assert (symbol? n))
          n))))

(define unbound?
  (lambda (x)
    (and (symbol? x)
         (let ((s (symbol->string x)))
           (and (> (string-length s) 2)
                (char=? #\_ (string-ref s 0))
                (char=? #\. (string-ref s 1)))))))

(define normalize
  (lambda (t)
    (if (list? t)
        (if (and (eq? 'vr (car t)) (null? (cddr t)))
            (normalize-var-name (cadr t))
            (map normalize t))
        (begin
          (assert (not (eq? 'vr t)))
          (if (unbound? t) `(quote ,t) t)))))

(define ok
  (lambda (r)
    (assert (not (null? r)))
    (map pretty-print r)
    (length r)))

(define quine
  '((lambda ((vr z)) (list (vr z) (list (quote quote) (vr z))))
    (quote (lambda ((vr z)) (list (vr z) (list (quote quote) (vr z)))))))

(ok
 (normalize
  (run* (q)
    (ev '()
        quine
        `(code ,quine)))))

(time
  (ok
 (map
  (lambda (q)
    (assert (equal? (scheme-eval q) q))
    q)
  (normalize
   (run 50 (q)
     (ev '()
         q
         `(code ,q)))))))

;(time
;(ok
 ;(map
  ;(lambda (ab)
    ;(let ((a (car ab))
          ;(b (cadr ab)))
      ;(assert (equal? (scheme-eval a) b))
      ;(assert (equal? (scheme-eval b) a))
      ;(list a b)))
  ;(normalize
   ;(run 5 (q)
     ;(fresh (a b)
       ;(== q `(,a ,b))
       ;(ev '() a `(code ,b))
       ;(ev '() b `(code ,a))))))))
