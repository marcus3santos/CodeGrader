;; q2.lisp

(deftest test-above ()
  (check
    (equalp (above 80 (list (make-record :name "Alice" :score 85)
                            (make-record :name "Bob" :score 92)
                            (make-record :name "Charlie" :score 78)
                            (make-record :name "David" :score 90)))
            (list (make-record :name "Alice" :score 85)
                  (make-record :name "Bob" :score 92)
                  (make-record :name "David" :score 90)))
    (equalp (above 70 (list (make-record :name "Alice" :score 80)
                            (make-record :name "Bob" :score 75)
                            (make-record :name "Charlie" :score 85)
                            (make-record :name "David" :score 70)))
            (list (make-record :name "Alice" :score 80)
                  (make-record :name "Bob" :score 75)
                  (make-record :name "Charlie" :score 85)))
    (equalp (above 90 (list (make-record :name "Alice" :score 90)
                            (make-record :name "Bob" :score 90)
                            (make-record :name "Charlie" :score 90)
                            (make-record :name "David" :score 90)))
            (list))))

(defun test-q2 ()
  (test-above)
  (fmakunbound 'above))

(test-q2)
