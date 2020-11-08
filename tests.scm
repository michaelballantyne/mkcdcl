(load "test-check.scm")

(test "nil-1"
  (run* (q) (== q '()))
  '(()))

(test "bool-1"
  (run* (q) (conde [(== q #t)] [(== q #f)]))
  '(#t #f))

(test "cons-1"
  (run* (q) (== q (cons 'a 'b)))
  '((a . b)))

#;(test "closure-1"
  (run* (q) (== q (make-closure 'x 'x '())))
  '(#(closure x x ())))

(test "int-1"
  (run* (q) (conde [(== q 1)] [(== q 2)]))
  '(1 2))

(test "real-1"
  (run* (q) (== q 2.5))
  '(2.5))

(test "fresh-1"
  (run* (q) (fresh (x y) (== x q) (== y q) (== x 1)))
  '(1))

(test "conde-1"
  (run* (q) (conde ((== q 1)) ((== q 2)) ((== q 3))))
  '(1 2 3))

(define (appendo l s out)
  (conde
    ((== l '()) (== s out))
    ((fresh (a d res)
       (== l (cons a d))
       (== out (cons a res))
       (appendo d s res)))))

(test "rec-1"
  (run 1 (q) (appendo '(a b) '(c d) q))
  '((a b c d)))

(test "rec-bwd-1"
  (run* (q) (fresh (x y) (appendo x y '(a b c d))
                   (== q (list x y))))
  '((() (a b c d))
    ((a) (b c d))
    ((a b) (c d))
    ((a b c) (d))
    ((a b c d) ())))


(define (anyo out)
  (conde
    ((== 1 out))
    ((anyo out))))

(test "anyo-1"
  (run 1 (q) (anyo q))
  '(1))

(test "cdcl-1"
  (run* (q)
    (fresh (x y)
      (conde
        ((== x 1))
        ((== x 2)))
      (conde
        ((== y 1))
        ((== y 2)))
      (== q (cons x y))))
  '((1 . 1) (2 . 1) (1 . 2) (2 . 2)))

(test "cdcl-2"
  (run* (q)
    (fresh (x)
      (conde
        ((== x 1))
        ((== x 2)))
      (== q x)))
  '(1 2))

(test "cdcl-3"
  (run* (q)
    (fresh (x y)
      (== x 1)
      (anyo y)
      (== x 2)))
  '())

(test "cdcl-4"
  (run* (q)
    (fresh (x y)
      (== x 1)
      (anyo y)
      (conde
        ((== x 2))
        ((== x 3)))))
  '())

(define (many1o x n)
  (if (<= n 0)
      (== x 1)
      (conde
        ((== x 1))
        ((many1o x (- n 1))))))

(define (manyn1o x n)
  (if (<= n 0)
      (== x 2)
      (conde
        ((== x (+ n 10)))
        ((manyn1o x (- n 1))))))

(test "cdcl-6"
  (run 1 (q)
    (fresh (x y z)
      (anyo x)
      (== y 1)
      (anyo z)
      (manyn1o y 1000))) ;; slow
  '())

(test "cdcl-7"
  (run 1 (q)
    (fresh (x y z)
      (anyo x)
      (== y 2)
      (anyo z)
      (many1o y 1000)))
  '())

(parameterize ([debug-soundness #t]
               [check-every 1])
  
  (test "cdcl-soundness-symbolo"
    (run* (q)
        (conde
          [(symbolo q)]
          [(numbero q)])
        (== q 1)
        )
    '(1))
  
  ; absento needs to track unifications it walks through
  ; when applying the constraint to ensure sound provenance.
  (test "absento-soundness-1"
    (run 1 (q)
      (fresh (x y z)
        (conde
          [(== x y)]
          [(== 'success 'success)])
        ; Note: this test works under the assumption that there is no
        ; suspension between the first unification and the final unification.
        ; Putting either of these constraints in a `fresh` will make the test
        ; ineffectual.
        (absento z x)
        (== y `(cat . ,z))))
    '(_.0))

  (test "absento-soundness-2"
    (run 1 (q)
      (fresh (y z)
        (conde
          [(== z 5)]
          [(== 'success 'success)])
        ; Note: this test works under the assumption that there is no
        ; suspension between the first unification and the final unification.
        ; Putting either of these constraints in a `fresh` will make the test
        ; ineffectual.
        (absento z y)
        (== y `(cat . 5))))
    '(_.0))
  
  ; When absento attributes a variable, it needs to include
  ; in the attributed information the provenance of the absento
  ; itself, and any unifications it walked through before
  ; attributing.
  (test "absento-soundness-3"
    (run 1 (q)
      (fresh (x)
        (conde
          [(absento 5 x)]
          [(== 'success 'success)])
        ; Note: this test works under the assumption that there is no
        ; suspension between the first absento and the final unification.
        ; Putting this constraint in a `fresh` will make the test
        ; ineffectual.
        (== x `(cat . 5))))
    '(_.0))
)


