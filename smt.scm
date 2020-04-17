(define smt-timeout (make-parameter 3))
(define smt-log-stmts (make-parameter #f))

;;(define smt-cmd "cvc4 --lang=smt2.6 -m --incremental --fmf-fun")
(define (smt-cmd) (format "z3 -in -t:~a" (smt-timeout)))

(define-values (smt-out smt-in smt-err smt-p) (values #f #f #f #f))
(define (sat/hard-reset!)
  (let-values (((out in err p)
                (process/text-ports (smt-cmd))))
    (set! smt-out out)
    (set! smt-in in)
    (set! smt-err err)
    (set! smt-p p)))

(define (sat/soft-reset!)
  (smt-call/forget '((reset))))

(define (smt-read-sat)
  (let ([r (read smt-in)])
    (cond
      ((eq? r 'sat)
       (set! sat-count (+ 1 sat-count))
       #t)
      ((eq? r 'unsat)
       (set! unsat-count (+ 1 unsat-count))
       #f)
      ((eq? r 'unknown)
       (set! unknown-count (+ 1 unknown-count))
       #t)
      (else (error 'read-sat (format "~a" r))))))

(define buffer '())
(define (smt-call xs)
  (set! buffer (cons xs buffer)))

(define (smt-call+flush-unbuffered stmtss)
  (for-each
      (lambda (stmts)
        (for-each
          (lambda (x)
            (when (smt-log-stmts)
              (printf "~s\n" x)
              (flush-output-port))
            (fprintf smt-out "~s\n" x))
          stmts))
      stmtss)
  (flush-output-port smt-out))

(define (smt-flush!)
  (let ([buffered (reverse buffer)])
    (smt-call+flush-unbuffered buffered))
  (flush-output-port smt-out)
  (set! buffer '()))

(define (smt-call/flush xs)
  (smt-call xs)
  (smt-flush!))

(define (smt-call/forget stmts)
  (smt-call+flush-unbuffered (list stmts))
  (set! buffer '()))

(define next-assumption-id 0)
(define (assumption-id->symbol id)
  (string->symbol (format "_a~a" id)))
(define (fresh-assumption-id!)
  (set! assumption-count (+ 1 assumption-count))
  (set! next-assumption-id (+ 1 next-assumption-id))
  (let ([id (assumption-id->symbol next-assumption-id)])
    (smt-call `((declare-const ,id Bool)))
    id))

(define (sat/constraint type v1 v2 v3)
  (smt-call (list `(assert (= ,v1 (,type ,v2 ,v3))))))

(define (check-sat-assuming vars)
  (smt-call/flush
   `((check-sat-assuming
      ,vars)))
  (smt-read-sat))

(define (sat/not-all prov)
  (smt-call (list `(assert (not (and . ,prov))))))

(define (sat/log-stats!)
  (void))
