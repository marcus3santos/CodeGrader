;; q2.lisp

(deftest test-interleave ()
  (check
    (equal (interleave '(a) '(d e f)) '(A D E F))
    (equal (interleave '(1 (2 3) 4) '(a b c d)) '(1 a (2 3) b 4 c d))
    (equal (interleave '() '()) ())
    (equal (interleave '(1 2) ()) '(1 2))
    (equal (interleave '(1 2 3) '(4)) '(1 4 2 3))))

(deftest test-comb ()
  (check
    (not (comb -10 2))
    (not (comb 3 5))
    (eql (comb 5 3) 10)
    (eql (comb 5 5) 1)
    (eql (comb 30 2) 435)))

(deftest test-mapf ()
  (check
    (equalp (mapf #'(lambda (x) (* x x)) '(1 2 3 4 5))
            '(1 4 9 16 25))
    (equalp (mapf #'(lambda (x) (string-upcase x)) '("casa" "arvore" "programa"))
            '("CASA" "ARVORE" "PROGRAMA"))
    (equalp (mapf #'(lambda (x) (1+ x)) nil) nil)))
			 
(defun test-q2 ()
  (test-comb)
  (test-interleave)
  (test-mapf)
  (fmakunbound 'comb)
  (fmakunbound 'interleave)
  (fmakunbound 'mapf))

(test-q2)
