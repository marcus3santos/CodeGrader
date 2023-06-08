;; q1.lisp


(deftest test-grade ()
  (check
    (equal (grade 60) "F")
    (equal (grade 78) "B+")
    (equal (grade 95) "A+")))


(defun test-q1 ()
  (test-grade)
  (fmakunbound 'grade))

(test-q1)
