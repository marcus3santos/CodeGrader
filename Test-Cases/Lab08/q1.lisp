;; q1.lisp

(deftest test-if()
  (check
   (equal (evaluate '(if t 1 2) nil) 1)
   (equal (evaluate '(if nil 1 2) nil) 2)
   (equal (evaluate '(if x y x) (list (cons 'x  nil) (cons 'y  1))) nil)
    (equal (evaluate '(if x y (+ y z)) (list (cons 'x  nil) (cons 'y   1) (cons 'z   2))) 3)
    (equal (evaluate '(if x y z) (list (cons 'x  t) (cons 'y  "Hello") (cons 'z  "World"))) "Hello")
   (equal (evaluate '(progn
                      (setf x 1)
                      (if x 
                        (setf x (+ x y)) 
                        x))
                      (list (cons 'x  nil) (cons 'y  1)))
          2)))

(deftest test-let ()
    (check
      (equal (evaluate '(let ((x 1)) x) nil) 1)
      (equal (evaluate '(let ((x 1)) x) '((x . 0))) 1)
      (equal (evaluate '(let ((x 1)) (setf x 2) x)
		       (list (cons 'x  0)))
	     2)
      (equal (evaluate '(let ((x 1) (y 2)) (+ x y)) nil)
	     3)
      (equal (evaluate '(let ((a 2)) (let ((b (+ a 4))) (+ a b))) nil)
	     8)
      (equal (evaluate '(let ((x (let ((x 2)) (* x 2)))) x) nil)
	     4)
      (equal (evaluate '(let ((x (* y 2))) (setf y (+ x 2)) (+ x y)) (list (cons 'y  3)))
	     14)
      (equal (evaluate '(let ((x 1) (y 2)) (if x x y)) nil)
	     1)
      (equal (evaluate '(let ((x 1) (y 2)) (if x (setf x (+ x z)) y) (+ x z)) (list (cons 'z  3)))
	     7)))

(defun test-q1 ()
  (test-if)
  (test-let)
  (fmakunbound 'test-if)
  (fmakunbound 'test-let))

(test-q1)
