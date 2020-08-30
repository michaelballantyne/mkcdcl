 (define (process/text-ports cmd)
   (open-process-ports cmd 'block (native-transcoder)))

(define scheme-eval
  (lambda (c)
    (eval c (environment '(rnrs)))))


; from srfi1 reference impl
(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))

(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error 'null-list? "argument out of domain" l))))

(define (find-tail pred list)
  (check-arg procedure? pred find-tail)
  (let lp ((list list))
    (and (not (null-list? list))
         (if (pred (car list)) list
           (lp (cdr list))))))

(define srfi1-member
  (case-lambda
    [(x lis)
     (srfi1-member x lis equal?)]
    [(x lis =)
     (find-tail (lambda (y) (= x y)) lis)]))

(define lset-union
  (let ((lset-union-2
          (lambda (= lis1 lis2)
            (cond ((null? lis1) lis2)    ; Don't copy any lists
                  ((null? lis2) lis1)    ; if we don't have to.
                  ((eq? lis1 lis2) lis1)
                  (else
                    (foldl (lambda (elt ans)
                             (if (srfi1-member elt ans =)
                               ans
                               (cons elt ans)))
                           lis1 lis2))))))
    (case-lambda
      ((=) (check-arg procedure? = lset-union) '())
      ((= lis1) (check-arg procedure? = lset-union) lis1)
      ((= lis1 lis2)
       (check-arg procedure? = lset-union)
       (lset-union-2 = lis1 lis2))
      ((= lis1 lis2 lis3 . lists)
       (check-arg procedure? = lset-union)
       (let* ((lis (lset-union-2 = lis1 lis2))
              (lis (lset-union-2 = lis  lis3)))
         (if (null? lists) lis
           (foldl (lambda (lis2 lis1) (lset-union-2 = lis1 lis2))
                  lis lists)))))))
