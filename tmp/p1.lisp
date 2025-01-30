(defun fact1 (n)
  (format t "just a test of ~a~%" n)
  (dotimes (i n)
    (if (> i 10)
        (return-from fact1 10))
    (format t "~a~%" i)))

(setf *test* 1)

(defun square (x) (* x x))
