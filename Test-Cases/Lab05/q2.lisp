;; q2.lisp

(deftest test-arith-eval ()
  (check
    (equal (arith-eval '([ 1 + 1 ])) 2)
    (equal (arith-eval '([ [ maxf 2 3 0 ] + 1 ])) 4)
    (equal (arith-eval '([ [ sdiv 8 2 3 ] + 1 ])) 3)
    (equal (arith-eval '([ fact [ 2 + 2 ] ])) 24)
    (equal (arith-eval '([ [ 2 ^ 3 ] - 2 ])) 6)
    (equal (arith-eval '([ [ 2 ^ [ 4 * 0 ] ] - 2 ])) -1)
    (equal (arith-eval '([ [ fact [ 4 + 5 ] ] + 1 ])) 362881)    
    (equal (arith-eval '([ [ maxf 2 -2 3 ] + [ sdiv 8 2 2 ] ])) 6)
    (equal (arith-eval '([ [ [ maxf -1 0 1 ] + [ maxf 4 3 0 ] ] + [ [ maxf 9 8 0 ] + [ maxf 16 12 9 ] ] ])) 30)
    (equal (arith-eval '([ [ [ sdiv 6 2 2 ] + [ sdiv 8 4 4 ] ] + [ 1 + [ 10 / 10 ] ] ])) 5)
    (equal (arith-eval '([ [ [ [ maxf 10000 1 9 ] + [ fact 1 ] ] + [ 1 + [ 10 / 10 ] ] ] - 1 ] ])) NIL)
    (equal (arith-eval '([ [ maxf 2 3 0 ] + 1 )) NIL)))


(defun test-q2 ()
  (test-arith-eval)
  (fmakunbound 'arith-eval))

(test-q2)
