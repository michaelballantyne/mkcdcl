(always-wrap-reified? #t)

(define one-of?
  (lambda (expected*)
    (lambda (produced)
      (not (not (member produced expected*))))))


(time-test "1"
 (run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           '(a b c d e)))
 '((_.0)))

(time-test "2"
(run* (q)
          (evalo
           `(letrec ((append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s))))))
              (append '(a b c) '(d e)))
           q))
'(((a b c d e))))

(time-test "3"
           (run 2 (q)
                (evalo
                  `(letrec ((append
                              (lambda (l s)
                                (if (null? l)
                                  s
                                  (cons (car l) (append (cdr l) s))))))
                     (append ,q '(d e)))
                  '(a b c d e)))
           '(('(a b c)) (((lambda _.0 '(a b c))) (=/= ((_.0 quote))) (sym _.0))))

(time-test "4"
           (run 1 (q)
                (evalo
                  `(letrec ((append
                              (lambda (l s)
                                (if (null? l)
                                  ,q
                                  (cons (car l) (append (cdr l) s))))))
                     (list
                       (append '(a b c) '(d e))))
                  '((a b c d e))))
           '((s)))

(time-test "5"
           (run 1 (q)
                (evalo
                  `(letrec ((append
                              (lambda (l s)
                                (if (null? l)
                                  s
                                  (cons ,q (append (cdr l) s))))))
                     (list
                       (append '() '(d e f))
                       (append '(c) '(d e f))
                       (append '(a b c) '(d e))))
                  '((d e f)
                    (c d e f)
                    (a b c d e))))
           '(((car l))))

(time-test "6"
           (run 1 (q)
                (absento 'a q)
                (absento 'b q)
                (absento 'c q)
                (absento 'd q)
                (absento 'e q)
                (absento 'f q)
                ;(== q '(append (cdr l) s))
                (evalo
                  `(letrec ((append
                              (lambda (l s)
                                (if (null? l)
                                  s
                                  (cons (car l) ,q)))))
                     (list
                       (append '() '(d e f))
                       (append '(c) '(d e f))
                       (append '(b c) '(d e f))
                       (append '(a b c) '(d e f))))
                  '((d e f)
                    (c d e f)
                    (b c d e f)
                    (a b c d e f))))
           '(((append (cdr l) s))))
