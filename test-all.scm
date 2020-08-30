(load "minisat-chez.scm")
(load "minisat.scm")
(load "mksat.scm")
(load "mk/mk-vicare.scm")
(load "mkcdcl-chez.scm")
(load "mkcdcl.scm")
(load "mk/mk-vicare.scm")
(load "mk/mk.scm")

(check-every 1)
(debug-soundness #t)

(load "test-check.scm")

(printf "==-tests\n")
(load "mk/==-tests.scm")

(printf "symbolo-tests\n")
(load "mk/symbolo-tests.scm")

(printf "numbero-tests\n")
(load "mk/numbero-tests.scm")

(printf "symbolo-numbero-tests\n")
(load "mk/symbolo-numbero-tests.scm")

(printf "stringo-tests.scm\n")
(load "mk/stringo-tests.scm")

(printf "disequality-tests\n")
(load "mk/disequality-tests.scm")

(printf "absento-closure-tests\n")
(load "mk/absento-closure-tests.scm")

(printf "absento-tests\n")
(load "mk/absento-tests.scm")

(printf "test-infer\n")
(load "mk/test-infer.scm")

(printf "test-simple-interp\n")
(load "mk/simple-interp.scm")
(load "mk/test-simple-interp.scm")

(printf "test-quines\n")
(load "mk/test-quines.scm")

(printf "test-numbers\n")
(load "mk/numbers.scm")
(load "mk/test-numbers.scm")
