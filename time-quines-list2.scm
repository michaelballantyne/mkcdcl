(define eval-expo
  (lambda (exp env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) exp)
         (absento 'closure v)
         (== v val)))
      ;((fresh (a*) ; n arg list
         ;(== `(list . ,a*) exp)
         ;(absento 'closure a*)
         ;(proper-listo a* env val)))
      ((fresh (e1 e2 v1 v2) ; 2 arg list
         (== exp `(list ,e1 ,e2))
         (== (list v1 v2) val)
         (absento 'closure e1)
         (absento 'closure e2)
         (eval-expo e1 env v1)
         (eval-expo e2 env v2)
         ))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) exp)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (== `(closure ,x ,body ,env) val))))))

(define proper-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d t-a t-d)
         (== `(,a . ,d) exp)
         (== `(,t-a . ,t-d) val)
         (eval-expo a env t-a)
         (proper-listo d env t-d))))))

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(define quine
  '((lambda (x) (list x (list (quote quote) x)))
    (quote (lambda (x) (list x (list (quote quote) x))))))

;(printf "10 quines\n")
;(time
  ;(run 10 (q) (eval-expo q '() q)))

;(printf "40 quines\n")
;(time
  ;(run 40 (q) (eval-expo q '() q)))

(time
  (run 50 (q) (eval-expo q '() q)))


;(printf "2 twines\n")
(time
  (run 2 (x) (fresh (p q)
         (=/= p q)
         (eval-expo p '() q)
         (eval-expo q '() p)
         (== `(,p ,q) x))))

;(printf "4 thrines\n")
;(time
  ;(run 4 (x)
       ;(fresh (p q r)
              ;(=/= p q)
              ;(=/= q r)
              ;(=/= r p)
              ;(eval-expo p '() q)
              ;(eval-expo q '() r)
              ;(eval-expo r '() p)
              ;(== `(,p ,q ,r) x))))



