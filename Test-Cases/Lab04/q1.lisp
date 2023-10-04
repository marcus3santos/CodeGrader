;; q1.lisp

(forbidden-functions :penalty 0.90 :functions '(sort))

(deftest test-sort-student-records ()
  (check
    (equalp (let* ((alice (make-record :name "Alice" :score 85))
                   (bob (make-record :name "Bob" :score 92))
                   (charlie (make-record :name "Charlie" :score 78))
                   (david (make-record :name "David" :score 90))
                   (student-records (list alice bob charlie david)))
              (sort-student-records student-records
                                    #'(lambda (x y) (> (record-score x) (record-score y)))))
            (let* ((alice (make-record :name "Alice" :score 85))
                   (bob (make-record :name "Bob" :score 92))
                   (charlie (make-record :name "Charlie" :score 78))
                   (david (make-record :name "David" :score 90)))
              (list bob david alice charlie)))
    (equalp (let* ((alice (make-record :name "Alice" :score 80))
                   (bob (make-record :name "Bob" :score 75))
                   (charlie (make-record :name "Charlie" :score 85))
                   (david (make-record :name "David" :score 70))
                   (student-records (list alice bob charlie david)))
              (sort-student-records student-records
                                    #'(lambda (x y) (< (record-score x) (record-score y)))))
            (let* ((alice (make-record :name "Alice" :score 80))
                   (bob (make-record :name "Bob" :score 75))
                   (charlie (make-record :name "Charlie" :score 85))
                   (david (make-record :name "David" :score 70))
                   (student-records (list alice bob charlie david)))
              (list david bob alice charlie)))
    (equalp (let* ((alice (make-record :name "Alice" :score 90))
                   (bob (make-record :name "Bob" :score 90))
                   (charlie (make-record :name "Charlie" :score 90))
                   (david (make-record :name "David" :score 90))
                   (student-records (list alice bob charlie david)))
             (sort-student-records student-records
                                   #'(lambda (x y) (string< (record-name x) (record-name y)))))
            (let* ((alice (make-record :name "Alice" :score 90))
                   (bob (make-record :name "Bob" :score 90))
                   (charlie (make-record :name "Charlie" :score 90))
                   (david (make-record :name "David" :score 90)))
              (list alice bob charlie david)))))
  

(defun test-q1 ()
  (test-sort-student-records)
  (fmakunbound 'sort-student-records))

(test-q1)


