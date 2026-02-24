(in-package :testing-runtime)

(defmacro is-safe (expression &key (timeout 1))
  "Evaluates the given expression and returns its result. If the expression
   does not complete within the specified timeout (in seconds), it returns
   a timeout error condition. If the expression triggers a stack overflow
   the store-condition captures that condition. Other kinds of errors are
   captured by the general ERROR condition"
  (let ((c (gensym)))
    `(handler-case
         (sb-ext:with-timeout ,timeout
           (fiveam:is ,expression))
       (sb-ext:timeout ()
         (fiveam:fail "Timeout: Infinite loop detected (> ~A sec)" ,timeout))
       (storage-condition ()
         (fiveam:fail "Memory error: Memory exhausted"))
       (error (,c)
         (fiveam:fail "Execution Error: ~A" ,c)))))

(defun summarize-results (q-label results)
  "Aggregates FiveAM result objects into a summarized report."

  (let ((total (length results))
        (passed (count-if (lambda (res) 
                            (typep res 'fiveam::test-passed)) 
                          results))
        (failures (remove-if (lambda (res) 
                               (typep res 'fiveam::test-passed)) 
                             results)))
    (list :q-label q-label
          :score (if (zerop total) 0 (* 100 (/ passed total)))
          :stats (list :total total :passed passed :failed (length failures))
          ;; Map the failures into a clean feedback list
          :feedback (mapcar (lambda (f)
                              (list :expr (fiveam::test-expr f)
                                    :reason (fiveam::reason f)
                                    :type (type-of f)))
                            failures))))

(defun q-label-p (label)
  (let ((str (symbol-name label)))
    (and (not (string= str "QUESTIONS"))
         (char= (aref str 0) #\Q))))

(defun get-assessment-test-case-data (assessment-data-file)
  "Return assoc list with relevant data from each question"
  (let* ((assessment-data (with-open-file (in assessment-data-file)
                            (read in))))
    (mapcar (lambda (qdata)
              (list (first qdata)
                    (remove-if-not (lambda (data)
                                     (or (eq data :solutions)
                                         (eq data :hidden)
                                         (eq data :given)
                                         (eq data :asked-function)))
                                   (second qdata) :key #'first)))
            (remove-if-not  #'q-label-p  assessment-data :key #'first)) ))


(defun grade-student (student-file q-label fname kind)
  (let ((runner-name (intern (format nil "RUN-~A-~A-~A-TEST" q-label fname kind))))
    ;; 1. Load student code
    
    (handler-case (with-package :sandbox
                    (load student-file))
      (error (c) (return-from grade-student 
                   (list :q-label q-label :error (format nil "Load Error: ~A" c)))))

    ;; 2. Execute the pre-compiled runner
    (let* ((raw-results (funcall runner-name))
           (summary (summarize-results q-label raw-results)))
      
      ;; 3. Log or Print the outcome
      (format t "~&Question ~A: ~D/~D passed (~A%)~%" 
              q-label 
              (getf (getf summary :stats) :passed)
              (getf (getf summary :stats) :total)
              (getf summary :score))
      
      summary)))

(defun process-entire-exam (student-file questions kind)
  "questions is a list of lists: ((:Q1 'fn1) (:Q2 'fn2))"
  (loop for (q-label fname) in questions
        collect (grade-student student-file q-label fname kind)))

(defun process-assessment-test-case-data (assessment-data-file q-labels-list)
  "Exposes the asked-functions in the sandbox (so :testing-runtime package
   can use them), and adds prefix package to the function name in the test-cases code.
   Returns an a-list ((q asked-function given hidden) ...) containing the 
   name of the function students were asked to implement, and the processed 
   code for the given and hidden test-cases"
  (let* ((assessment-data (get-assessment-test-case-data assessment-data-file))
         (testcase-data (mapcar (lambda (q)
                                  (list q
                                        :asked-function (second (assoc :asked-function (second (assoc q assessment-data))))
                                        :given (second (assoc :given (second (assoc q assessment-data))))
                                        :hidden (second (assoc :hidden (second (assoc q assessment-data))))))
                              q-labels-list)))
    (mapc (lambda (d)
            (unintern  (getf (rest d) :asked-function) :testing-runtime)
            (export (list (intern (symbol-name (getf (rest d) :asked-function)) :sandbox)) :sandbox))
          testcase-data)
    (mapcar (lambda (tc-data)
              (let* ((q-label (first tc-data))
                     (props (rest tc-data)))
                (list q-label
                      :asked-function (getf props :asked-function) 
                      :given (add-prefix-to-symbol-in-form (getf props :given) (getf props :asked-function) :sandbox)
                      :hidden (add-prefix-to-symbol-in-form (getf props :hidden) (getf props :asked-function) :sandbox))))
            testcase-data)))

