(always-wrap-reified? #t)

(define one-of?
  (lambda (expected*)
    (lambda (produced)
      (not (not (member produced expected*))))))

(check-every 1)

(time-test "1"
  (run 1 (q)
    (evalo q 5)
    (evalo 5 6))
  '())

(time-test "2"
  (run 1 (q)
    (evalo q 5)
    (evalo 'a 6))
  '())

(time-test "2b"
  (run 1 (q)
    (evalo q 5)
    (evalo '(lambda (a) b) 6))
  '())

(time-test "2c"
  (run 1 (q)
    (evalo q 5)
    (lookupo 'list '() 5))
  '())

(time-test "3a"
  (run 1 (q)
    (evalo q 5)
    (lookupo 'list `((list . (val 6))) 5))
  '())

(time-test "3b"
  (run 1 (q)
    (evalo q 5)
    (lookupo 'list initial-env 5))
  '())

(time-test "3"
  (run 1 (q)
    (evalo q 5)
    (evalo 'list 6))
  '())

(time-test "4"
  (run 1 (q)
    (evalo q 5)
    (evalo 'car 6))
  '())

(time-test "5a"
  (run 1 (q)
    (fresh (e1 e2)
      (evalo `(cons ,e1 ,e2) 6)))
  '())

(time-test "5b"
  (run 1 (q)
    (fresh (e1 e2)
      (evalo `(cons ,e1 ,e2) 6)
      (evalo q 5)))
  '())

(time-test "5c"
  (run 1 (q)
    (evalo q 5)
    (fresh (e1 e2)
      (evalo `(cons ,e1 ,e2) 6)))
  '())
