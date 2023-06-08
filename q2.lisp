;; q2.lisp

(defun ck-events (elr &optional (el (reverse elr)) (count 0))
  (cond ((null (cdr el)) count)
        ((> (caadr el) (caar el)) (ck-events elr (cdr el) (1+ count)))
        (t (ck-events el (cdr el) count))))

(defun execute-experiment (n)
  (setf *events* (list))
  (simulate-dice-rolls n))

(deftest test-simulate-dice-rolls ()
  (check
    (equal (execute-experiment 5)
           (ck-events *events*))
    (equal (execute-experiment 25)
           (ck-events *events*))
    (equal (execute-experiment 2)
           (ck-events *events*))
    (equal (execute-experiment 13)
           (ck-events *events*))
    (equal (execute-experiment 100)
           (ck-events *events*))))

(defun test-q2 ()
  (test-simulate-dice-rolls)
  (fmakunbound 'simulate-dice-rolls))

(test-q2)
