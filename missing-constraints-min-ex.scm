(always-wrap-reified? #t)
(check-every 1)

(define (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . (val ,b)) . ,rest) env)
    (conde
      ((== x y)
       (== b t))
      ((=/= x y)             ; This fails in miniKanren, but reports nothing to the solver as we haven't implemented that yet.
       (lookupo x rest t))   ; As a result, this never runs.
      ; So the solver knows nothing about this disjunct. In the future it may well succeed,
      ; as far as it knows, with any value of t.
      )))

(define (anyo out)
  (conde
    ((== 1 out))
    ((anyo out))))

(test "good-lookup"
  (run 1 (q)
    (lookupo 'list `((list . (val 6))) 6))
  '((_.0)))

(test "unbound-lookup"
  (run 1 (q)
    (lookupo 'car `((list . (val 6))) 6))
  '())

(test "bad-val-lookup"
  (run 1 (q)
    (lookupo 'list `((list . (val 6))) 5))
  '())

; We'd like this to terminate, but it doesn't.
(test "anyo-bad-val-lookup"
  (run 1 (q)
    (anyo q)
    (lookupo 'list `((list . (val 6))) 5))
  '())
