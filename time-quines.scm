(define eval-expo
  (lambda (exp env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) exp)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (== v val)))
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (proper-listo a* env val)))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) exp)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(closure ,x ,body ,env) val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((== '() env)))))

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

(printf "10 quines\n")
(time
  (run 10 (q) (eval-expo q '() q)))

(printf "40 quines\n")
(time
  (run 40 (q) (eval-expo q '() q)))

(printf "2 twines\n")
(time
  (run 2 (x) (fresh (p q)
	       (=/= p q)
	       (eval-expo p '() q)
	       (eval-expo q '() p)
	       (== `(,p ,q) x))))

(printf "4 thrines\n")
; check-every 8:
; cpu time: 36654 real time: 36864 gc time: 2781
; check-every #f:
; cpu time: 8065 real time: 8034 gc time: 2435
; Not a win. :(
(time
  (run 4 (x)
       (fresh (p q r)
              (=/= p q)
              (=/= q r)
              (=/= r p)
              (eval-expo p '() q)
              (eval-expo q '() r)
              (eval-expo r '() p)
              (== `(,p ,q ,r) x))))



