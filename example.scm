;(check-every #f) ; 220 seconds
(check-every 8) ; 40 seconds

; So CDCL does something, in some cases! That's good. Maybe if we can
; make the overhead low enough, it'll be useful.

; Oh. But 35 seconds in faster-miniKanren. So we're still behind.
; Maybe if I make it harder we'll come out ahead.

(log-stats #t)

(load "test-check.scm")
(load "mk/full-interp.scm")

(time-test "synth-branches"
(run 1 (e1 e2)
     (fresh (any1 any2)
(evalo
 `(letrec ([f (lambda (x)
              (if (symbol? x)
                ,e1
                ,e2))])
    (list
      (f 'x)
      ,any1
      (f 'y)))
(list
 '((x) x)
 (list 'e)
 '((y) y)))))
'((x (cdr x))))
