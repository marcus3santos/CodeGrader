;; q1.lisp

(deftest test-square ()
  (check
    (equal (square 0) 0)
    (equal (square -1) 1)
    (equal (square 4) 16)
    (equal (square 40) 1600)
    (equal (square -10) 100)
    (equal (square 0.4) (* 0.4 0.4))))

(defun test-q1 ()
  (test-square)
  (fmakunbound 'square))

(test-q1)
