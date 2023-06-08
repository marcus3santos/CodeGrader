;; q3.lisp

(deftest test-enter-garage ()
  (check
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (enter-garage 1))
          1)
   (equal (progn
            (max-capacity 2 3)
            (enter-garage 2)
            (enter-garage 2))
          1)
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (enter-garage 1)
            (enter-garage 1)
            (enter-garage 1))
          0)
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (exit-garage 1)
            (enter-garage 1)
            (enter-garage 1))
          1)
   (equal (progn
            (max-capacity 2 3)
            (enter-garage 2)
            (exit-garage 2)
            (enter-garage 2))
          2)))

(deftest test-exit-garage ()
  (check
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (enter-garage 1)
            (exit-garage 1))
          2)
   (equal (progn
            (max-capacity 2 3)
            (enter-garage 2)
            (exit-garage 2))
          3)
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (enter-garage 1)
            (exit-garage 1)
            (exit-garage 1))
          3)
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (exit-garage 1)
            (enter-garage 1)
            (exit-garage 1))
          3)
   (equal (progn
            (max-capacity 2 3)
            (enter-garage 2)
            (enter-garage 2)
            (enter-garage 2)
            (enter-garage 2)
            (enter-garage 2)
            (exit-garage 2))
          1)))

(defun test-q3 ()
  (test-exit-garage)
  (test-enter-garage)
  (fmakunbound 'exit-garage)
  (fmakunbound 'enter-garage)
  )

(test-q3)
