;; q2.lisp

(deftest test-fact ()
  (check
    (equal (fact 0) 1)
    (equal (fact 1) 1)
    (equal (fact 5) 120)
    (equal (fact 4) 24)
    (equal (fact 20) 2432902008176640000)))

(defun test-q2 ()
  (test-fact)
  (fmakunbound 'fact))

(test-q2)
