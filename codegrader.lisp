
(in-package #:codegrader)

(defstruct submission
  std-id
  std-fname
  std-lname
  room-pc
  std-name
  date
  evaluation ; percentage marks per question and explanations
  total-marks) ; total marks, i.e., (sum correctness marks per question)/(Number of questions)

;; Folder in student's home directory storing their solutions
(defparameter *std-sub-folder* "pt/")


;; Root folder where the examples' test case files  and the names of the
;; assessment functions are stored.
;; The actual files should be inside the PT1/ or PT2/ folder, as appropriate

(defparameter *examples-folder* "~/Codegrader/Examples/")

(defparameter *assessment-funcs-folder* "~/Codegrader/Assessment-funcs/")
;;(defparameter *sandbox-pkg-folder* "~/Codegrader/Sandbox/")

;; List of assessments and labs

(defparameter *assessments* '("lab01" "lab02"
                              "lab03" "lab04"
                              "lab05" "lab06"
                              "lab07" "lab08"
                              "lab09" "pt1" "pt2"))

(defun check-input-files (lf)
  (when lf
    (if (probe-file (car lf))
	(check-input-files (cdr lf))
	(error "Folder/file ~S does not exist." (car lf)))))

(defun clean-symbol-names (e)
  (cond ((symbolp e) (intern (symbol-name e)) )
        ((consp e) (cons (clean-symbol-names (car e))  (clean-symbol-names (cdr e))))
        (t e)))

(defun gen-message (r)
  (let* ((res (first r))
         (rt-error (when (second r)
                     (symbol-name (class-name (class-of (second r))))))         
         (test (nth 3 r))
         (fcall (second test))
         (ret (third test)))
    (cond ((string= res "Pass")
           (format nil "Passed: ~a returned ~a" fcall ret))
          ((and (string= res "Fail") rt-error) (format nil "Failed: ~a when evaluating ~a." rt-error fcall))
          (t (format nil "Failed: ~a did not return ~a." fcall  ret)))))

(defun generate-messages (out eval)
  (format out "--EVALUATION FEEDBACK--~%~%NOTE:~%- Each question is worth 100 points.~%- Your score is the sum of your questions' points divided by the number of questions in the assessment.~%~%")
  (format out "Your score: ~a (out of 100)~%" (car eval))
  (dolist (question (cadr eval))
    (let* ((q (car question))
           (qeval (cadr question))
           (mark (first qeval))
           (error-type (second qeval))
	   (descr (third qeval))
	   (res (clean-symbol-names (fourth qeval)))
           (std-sol (nth 4 qeval))
           (unit-test-name (symbol-name (third (car res))))
           (pos (position #\- unit-test-name))
           (func-name (if pos (subseq unit-test-name (1+ pos))
                          unit-test-name)))
      (format out "~%---------------------------------------------------------------------------~%* ~a: ~a points (out of 100).~%" q mark)
      (unless (or (equalp error-type "missing-question-file")
                 (equalp error-type "no-submitted-file")
		 (equalp error-type "not-lisp-file")
		 (equal error-type "late-submission"))
        (format out "~%------ Your Solution ------~%~%~A~%~%--- End of Your Solution --~%" std-sol))
      (cond ((or (equalp error-type "missing-question-file")
                 (equalp error-type "no-submitted-file")
		 (equalp error-type "not-lisp-file")
		 (equal error-type "late-submission")) 
             (format out "~%~A !!!" descr))
            ((and (listp error-type) (string= (car error-type) "used forbidden symbol"))
             (format out "~%!!! You have used a forbidden symbol, ~A, in your Lisp file !!!~%" (cadr error-type))
             (format out "~%Your mark for all parts of this question was reduced by ~a% for using forbidden symbol.~%" (* (caddr error-type) 100))
             (format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}" func-name (mapcar #'gen-message res)))
	    ((equal error-type "No RT-error") 
             (format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}" func-name (mapcar #'gen-message res)))
	    (t (format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}" func-name (mapcar #'gen-message res))))))
  (format out "~%---------------------------------------------------------------------------")
  (format out "~%--END OF EVALUATION FEEDBACK--~%~%"))

(defun generate-feedback (key eval feedback-folder)
  (let* ((fname (concatenate 'string key ".txt"))
	 (folder (ensure-directories-exist (concatenate 'string  (namestring feedback-folder) fname))))
    (with-open-file (out folder :direction :output :if-exists :supersede)
      (generate-messages out eval))))

(defun generate-d2l-feedback (key eval feedback-folder)
  (let* ((fname (concatenate 'string (subseq key 0 (1- (length key))) ".txt"))
	 (folder (ensure-directories-exist (concatenate 'string  (namestring feedback-folder) fname))))
    (with-open-file (out folder :direction :output :if-exists :supersede)
      (generate-messages out eval))))

(defun get-std-id (csv)
  (subseq csv 1 (position #\, csv)))

(defun get-std-name (csv)
  (let* ((pref1 (subseq csv (1+ (position #\, csv))))
	 (pref2 (subseq pref1 (1+ (position #\, pref1))))
	 (lname (subseq pref1 0 (position #\, pref1)))
	 (fname (subseq pref2 0 (position #\, pref2))))
    (concatenate 'string fname " " lname)))

(defun change-mark-csv (csv mark)
  (let* ((pref1 (subseq csv 0 (position #\, csv :from-end 0)))
	 (pref2 (subseq pref1 0 (position #\, pref1 :from-end 0))))
    (concatenate 'string pref2 "," (write-to-string mark) ",#")))

(defun get-insert-exam-grade (log-file-stream stream csv ht f)
  "This version uses the student id # as hash key"
  (let* ((std-id (get-std-id csv))
	 (v (gethash std-id ht)))
    (if v
        (let ((new-mark (change-mark-csv csv (funcall f v)))
              (std-name (concatenate 'string (submission-std-fname v) " " (submission-std-lname v))))
          (format log-file-stream "Mark of student ~a (~a) changed from ~a to ==> ~a~%" std-name std-id csv new-mark)
          (format stream "~A~%"  new-mark))
	(progn 
          (format log-file-stream "Student ~a did not submit solution!~%" std-id)))))

(defun get-insert-grade (log-file-stream stream csv ht f)
  (let* ((std-name (get-std-name csv))
	 (v (gethash std-name ht)))
    (if v
        (let ((new-mark (change-mark-csv csv (funcall f v))))
          (format log-file-stream "Mark of student ~a changed from ~a to ==> ~a~%" std-name csv new-mark)
          (format stream "~A~%"  new-mark))
	(progn 
          (format log-file-stream "~S did not submit solution!~%" std-name)
          (format *standard-output* "~A~%" std-name)))))

(defun generate-exam-marks-spreadsheet (log-file-stream d2l-file folder ht f out-file)
  (when d2l-file
    (with-open-file (in d2l-file :direction :input)
      (with-open-file (out (merge-pathnames folder out-file)
			   :direction :output :if-exists :supersede)
        (format out "~A~%" (read-line in nil))
        ;(format *standard-output* "Students that did not write a solution are listed below:~%")
        (loop for line = (read-line in nil)
	      while line do
	        (get-insert-exam-grade log-file-stream out line ht f))))))

(defun generate-marks-spreadsheet (log-file-stream d2l-file folder ht f out-file)
  (when d2l-file
    (with-open-file (in d2l-file :direction :input)
      (with-open-file (out (merge-pathnames folder out-file)
			   :direction :output :if-exists :supersede)
        (format out "~A~%" (read-line in nil))
        (format *standard-output* "Students that did not submit solution are listed below:~%")
        (loop for line = (read-line in nil)
	      while line do
	        (get-insert-grade log-file-stream out line ht f))))))

(defun check-input-files (lf)
  (when lf
    (if (probe-file (car lf))
	(check-input-files (cdr lf))
	(error "Folder/file ~S does not exist." (car lf)))))

(defun cleanup-folder (folder)
  (if  (probe-file folder)
       (sb-ext:delete-directory folder :recursive t)))

(defun replace-char (s c r)
  (setf (aref s (position c s)) r)
  s)

(defun consume-until (s c)
  (subseq s (1+ (position c s))))

(defun check-foldername (p)
  "Adds a / to the end of the folder name if it is not there already"
  (if (char= (aref p (1- (length p))) #\/)
      p
      (concatenate 'string p "/")))

(defun create-folder-ifzipped (is-zipped submissions-dir unzipped-subs-folder)
  (if is-zipped
      (progn
        (ensure-directories-exist unzipped-subs-folder :verbose T)
	(zip:unzip submissions-dir unzipped-subs-folder :if-exists :supersede)
	unzipped-subs-folder)
      (check-foldername submissions-dir)))

(defun get-date-time()
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
	(get-decoded-time)
      dst-p
      (format nil "It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
	      hour
	      minute
	      second
	      (nth day-of-week day-names)
	      month
	      date
	      year
	      (- tz)))))

(defun get-key-and-date (folder)
  (let* ((s (namestring folder))
	 (sn (subseq s 0 (- (length s) 1)))
	 (pre (splice-at-char sn #\/)))
    (multiple-value-bind (date p2) (splice-at-char pre #\-)
	(values (subseq pre 0 p2) (subseq date 1)))))

(defun splice-at-char (s c)
  (let ((i (position c s :from-end 0)))
    (values (subseq (subseq s i) 1) i)))

(defun form-date-time (date)
  (with-input-from-string (in date)
    (let ((month (read in))
	  (day (prog1 (read in)
		 (read in)))
	  (time (read in))
	  (period (read in)))
      (list month day time period))))

(defun check-dt (dt)
  (let ((m (car dt))
	(d (cadr dt))
	(td (if (<= (caddr dt) 12) (* (caddr dt) 100) (caddr dt)))
	(p (cadddr dt)))
    (list m d td p)))


(defun get-solution (fname lfiles)
  (if (string= fname (file-namestring (car lfiles)))
      (car lfiles)
      (get-solution fname (cdr lfiles))))

(defun remove-extension (filename)
  (subseq filename 0 (position #\. filename :from-end t)))



(defun grade-solutions (solution-files test-cases-files)
  (let ((results (list))
        (sol-fnames (mapcar #'file-namestring solution-files)))
    (dolist (test-case test-cases-files)
      (push (list (remove-extension (file-namestring test-case))
                  (if (member (file-namestring test-case) sol-fnames :test #'string=)
                      (let* ((solution (get-solution (file-namestring test-case) solution-files))
                             (evaluation (evaluate-solution solution test-case)))
                        evaluation)
                      (list 0 "missing-question-file" (concatenate 'string (file-namestring test-case) " file not found" nil))))
            results))
    (reverse results)))


(defun parse-room-pc (str)
  (do ((i 0 (1+ i)))
      ((or (= i (length str))
           (and (not (alphanumericp (aref str i)))
                (not (char= #\- (aref str i)))))
       (subseq str 0 i))))

(defun get-insert-std (line table)
  (let* ((id (subseq line 0 (position #\, line)))
         (sufx1 (subseq line (1+ (length id))))
         (fname (subseq sufx1 0 (position #\, sufx1)))
         (sufx2 (subseq sufx1 (1+ (length fname))))
         (lname (subseq sufx2 0 (position #\, sufx2)))
         (room-pc (parse-room-pc (subseq sufx2 (1+ (length lname))))))
    (setf (gethash room-pc table) (list id fname lname room-pc))))

(defun create-mapping-table (csv-file)
  (let ((htable (make-hash-table :test 'equal)))
    (with-open-file (in csv-file :direction :input)
      (loop for line = (read-line in nil)
            while line do
              (get-insert-std line htable)))
    htable))

(defun export-functions (file)
  (let ((current *package*))
    (in-package :sandbox)
    (with-open-file (in file :direction :input)
      (dolist (fname (read in))
        (export (intern (symbol-name fname) :sandbox))))
    (setf *package* current)))

;; -------- Not integrated to the CodeGrader yet
(defun chk-my-solution (a# q#)
  "Q# is a string identifying a question, e.g., \"q1\".
   A# is a string identifying the assessment name, e.g., \"lab01\", \"pt1\", \"pt2\", etc.
   Checks if the student's solution is in the required folder defined in *std-sub-folder*
   and with the required file name, i.e., (concatenate 'string q# \".lisp\"),
   and runs the solution against the given examples for that question stored in
   *pt-examples-folder*/q#.lisp."
  (let* ((folder (format nil "~a~a" (namestring (user-homedir-pathname)) *std-sub-folder*))
         (fname (format nil "~a.lisp" q#))
         (folder-file (format nil "~a~a" folder fname))
         (testcase-file (car (directory (format nil "~a~a/~a" *examples-folder* (string-upcase a#) fname))))
         (sandbox-functions-file (car (directory (format nil "~a~a/~a" *assessment-funcs-folder* (string-upcase a#) "assessment-functions.lisp"))))
         ;;(sandbox-pkg-file (car (directory (format nil "~a~a/~a" *sandbox-pkg-folder* (string-upcase a#) "sandbox-runtime-package.lisp"))))
         (current-pckg *package*))
    (unless (member a# *assessments* :test #'string=)
      (error "~%!!! Assessment/Lab does not exist !!!"))
    (unless testcase-file
      (error "~%!!! Test case file or folder does not exist !!!"))
    (unless (probe-file folder-file)
      (error "~%!!! File ~a does not exist in folder ~S !!!" fname folder))
    ;; Should instead intern and export the assessment's function names to the sandbox package
    (export-functions sandbox-functions-file)
    ;;(load sandbox-pkg-file)
    (unwind-protect 
         (let* ((eval (evaluate-solution folder-file (namestring testcase-file)))
                (error-type (second eval)))
           ;;(format t "---- ~s~%" eval)
           (when (and (listp error-type) (string= (car error-type) "used forbidden symbol"))
             (format t "~%!!! You have used a forbidden symbol, ~A, in your Lisp file !!!~%" (cadr error-type))
             (format t "~%Your mark for all parts of this question will be reduced by ~a% for using a forbidden symbol.~%" (* (caddr error-type) 100)))
           (if (and (stringp error-type) (string= error-type "runtime-error"))
               (format t "~a" (nth 2 eval))
               (format t "~%When testing your solution for ~A, the results obtained were the following:~%~{- ~a~%~}" q# (mapcar #'gen-message (nth 3 eval)))))
      (setf *package* current-pckg))
    t))
;;--0--------

(defun grade-exam (submissions-zipped-file std-pc-map tests-folder results-folder &optional exam-grades-export-file)
  "submissions-zipped-file is the zipped file containing the student solutions
   pc-std-map is a csv file containing the student ID, name, and room-machine ID"
  (check-input-files (append (when exam-grades-export-file (list exam-grades-export-file)) (list submissions-zipped-file std-pc-map tests-folder)))
  (let* ((results-folder (check-foldername  (namestring (ensure-directories-exist results-folder :verbose T))))
         (test-cases-folder (check-foldername  (namestring (ensure-directories-exist tests-folder :verbose T))))
         (feedback-folder (merge-pathnames "student-feedback/" results-folder))
                                        ;(feedback-zipped (merge-pathnames results-folder "student-feedback.zip"))
	 (subs-folder (merge-pathnames "submissions/" results-folder))
	 (subs-folder-wfiles (progn
                               (cleanup-folder feedback-folder)
                               (cleanup-folder subs-folder)
                               (uiop:run-program (concatenate 'string "unzip " (namestring submissions-zipped-file) " -d " (namestring subs-folder)))
                                        ;(zip:unzip submissions-zipped-file subs-folder :if-exists :supersede)
	                       subs-folder))
	 (sfolders (directory (concatenate 'string (namestring subs-folder-wfiles) "*/")))
         (map (create-mapping-table std-pc-map)))
    (with-open-file (log-file-stream (ensure-directories-exist (merge-pathnames "codegrader-history/log.txt" (user-homedir-pathname)))
                                     :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
      (let ((broadcast-stream (make-broadcast-stream *standard-output* log-file-stream)))
        (format broadcast-stream "~a: Started marking~%" (get-date-time))
        (dolist (folder sfolders)
          (let* ((str (namestring folder))
                 (temp (subseq str (1+ (position #\/ (subseq str 0 (1- (length str))) :from-end t))))
                 (room-pc (subseq temp 0 (1- (length temp))))
                 (std (gethash room-pc map)))
            (when std
              (format t "Running program of student ~a~%" std)
              (let* ((student-files (directory (concatenate 'string (namestring  folder) *std-sub-folder* "*.*")))
                     (test-cases-files (directory (merge-pathnames test-cases-folder "*.lisp")))
                     (solutions-evaluations (if test-cases-files (grade-solutions student-files test-cases-files)
                                                (error "There are no lisp files in the provided test cases folder!!!")))
                     (seval (list (/ (reduce #'+ solutions-evaluations :key #'caadr) (length test-cases-files))
                                  solutions-evaluations))
                     (item (make-submission :std-id (first std)
                                            :std-fname (second std)
                                            :std-lname (third std)
                                            :room-pc (fourth std)
                                            :evaluation seval
                                            :total-marks (car seval)))
                     (anony-id (format nil "~A" (sxhash (submission-std-id item)))) ;; hashes the student ID#
                                        ;(anony-id (subseq (submission-std-id item) 5))
                     )
                (format log-file-stream "Student ~a (~a ~a),  result:~%~a~%" (submission-std-id item) folder (concatenate 'string anony-id ".txt") seval)
                (setf (gethash (submission-std-id item) map) item)
                (generate-feedback anony-id seval feedback-folder)))))
        (in-package :codegrader)
        (format *standard-output* "~%============================================================================~%")
        (format *standard-output* "Slime produced the above messages when loading the students' solutions~%")
        (format *standard-output* "============================================================================~%")
        (format broadcast-stream "Done marking students solutions.~%")
                                        ;(format broadcast-stream "Generating the zipped feedback folder...~%")
                                        ;(zip:zip feedback-zipped feedback-folder :if-exists :supersede)
                                        ;(format broadcast-stream "Done.~%")
        (when exam-grades-export-file (format broadcast-stream "Generating the grades spreadsheet...~%"))
        (generate-exam-marks-spreadsheet log-file-stream exam-grades-export-file results-folder map #'(lambda (x) (submission-total-marks x)) "grades.csv")
        (when exam-grades-export-file (format broadcast-stream "Done.~%"))
        (sb-ext:delete-directory (namestring subs-folder) :recursive t)
        (format broadcast-stream "Exam grading complete!~%" )
        (format *standard-output* "You may now upload to D2L the following grade files stored in your ~a folder :~%" results-folder)
        (when exam-grades-export-file
          (format *standard-output* "- grade.csv : contains the test marks~%"))
        (format *standard-output* "- student-feedback/ : contains the feedback txt files for each student.")
        (in-package :cl-user)
        "(^_^)"))))

(defun grade-it (submissions-zipped-file tests-folder results-folder &optional exam-grades-export-file)
  (check-input-files (append (when exam-grades-export-file (list exam-grades-export-file)) (list submissions-zipped-file tests-folder)))
  (let* ((results-folder (check-foldername  (namestring (ensure-directories-exist results-folder :verbose T))))
         (test-cases-folder (check-foldername  (namestring (ensure-directories-exist tests-folder :verbose T))))
         (feedback-folder (merge-pathnames "student-feedback/" results-folder))
	 (feedback-zipped (merge-pathnames results-folder "student-feedback.zip"))
	 (subs-folder (merge-pathnames "submissions/" results-folder))
	 (subs-folder-wfiles (progn
                               (cleanup-folder feedback-folder)
                               (cleanup-folder subs-folder)
	                       (zip:unzip submissions-zipped-file subs-folder :if-exists :supersede)
	                       subs-folder))
	 (sfolders (directory (concatenate 'string (namestring subs-folder-wfiles) "*/")))
	 (h-table (make-hash-table :test 'equal)))
    (with-open-file (log-file-stream (ensure-directories-exist (merge-pathnames "codegrader-history/log.txt" (user-homedir-pathname)))
                                     :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
      (let ((broadcast-stream (make-broadcast-stream *standard-output* log-file-stream)))
        (format broadcast-stream "~a: Started marking~%" (get-date-time))
        (dolist (folder sfolders)
          (multiple-value-bind (key date) (get-key-and-date folder)
            (let* ((pref (consume-until (consume-until key #\-) #\-)) ;(splice-at-char key #\-))
                   (std-name (subseq pref 1 (1- (length pref))))
                   (sdate (check-dt (form-date-time (replace-char date #\, #\ ))))
                   (student-files (directory (merge-pathnames folder "*.*")))
                   (test-cases-files (directory (merge-pathnames test-cases-folder "*.lisp")))
                   (solutions-evaluations (grade-solutions student-files test-cases-files))
                   (seval (list (/ (reduce #'+ solutions-evaluations :key #'caadr) (length test-cases-files))
                                solutions-evaluations))
                   (item (make-submission :std-name std-name
                                          :date sdate
                                          :evaluation seval
                                          :total-marks (car seval))))
              (format log-file-stream "Student *~a*,  result:~%~a~%" std-name seval)
              (setf (gethash std-name h-table) item)
              (generate-d2l-feedback key seval feedback-folder)
              )))
        (in-package :codegrader)
        (format *standard-output* "~%============================================================================~%")
        (format *standard-output* "Slime produced the above messages when loading the students' solutions~%")
        (format *standard-output* "============================================================================~%")
        (format broadcast-stream "Done marking students solutions.~%")
        (format broadcast-stream "Generating the zipped feedback folder...~%")
        (zip:zip feedback-zipped feedback-folder :if-exists :supersede)
        (format broadcast-stream "Done.~%")
        (when exam-grades-export-file (format broadcast-stream "Generating the grades spreadsheet...~%"))
        (generate-marks-spreadsheet log-file-stream exam-grades-export-file results-folder h-table #'(lambda (x) (submission-total-marks x)) "grades.csv")
        (when exam-grades-export-file (format broadcast-stream "Done.~%"))
        (format broadcast-stream "Exam grading complete!~%" )
        (format *standard-output* "You may now upload to D2L the following grade files stored in your ~a folder :~%" results-folder)
        (when exam-grades-export-file
          (format *standard-output* "- grade.csv : contains the test marks~%"))
        (format *standard-output* "- student-feedback.zip : contains the feedback txt files for each student.")
        (in-package :cl-user)
        "(^_^)"))))
        

(defun get-lab-files (lab)
  (directory (merge-pathnames (concatenate 'string "Test-Cases/" lab "/*.lisp") (asdf:system-source-directory :codegrader))))

(defun my-feedback-file (stdid)
  (format nil "~A.txt" (sxhash (format nil "~A" stdid))))

(defun eval-student-solutions (std-id solutions-folder test-cases-folder output-folder)
  "Based on the given student id (std-id, an integer), the students' solutions in solutions-folder, and 
   the test cases in test-cases-folder, generates a file in the output-folder containing the CodeGrader generated feedback."
  (let* ((fname (concatenate 'string (format nil "~A" (sxhash (format nil "~A" std-id))) ".txt"))
	 (folder (ensure-directories-exist (concatenate 'string  (namestring output-folder) fname))))
    (with-open-file (out folder :direction :output :if-exists :supersede)
      (eval-solutions solutions-folder :exam test-cases-folder out))
     (format t "Feedback saved in ~a~%" folder)))

(defun eval-solutions (solutions-folder lab &optional  test-cases-folder (out t))
  (unless (probe-file solutions-folder)
    (error "Folder does not exist: ~S~%" solutions-folder))
  (if test-cases-folder
      (unless (probe-file test-cases-folder)
        (error "Folder does not exist: ~S~%" test-cases-folder)))
  (let* ((solution-files (directory (merge-pathnames solutions-folder "*.*")))
         (test-cases-files
           (if test-cases-folder
               (directory (concatenate 'string test-cases-folder "*.lisp"))
               (case lab
                 (:lab01 (get-lab-files "Lab01"))
                 (:lab02 (get-lab-files "Lab02"))
                 (:lab03 (get-lab-files "Lab03"))
                 (:lab04 (get-lab-files "Lab04"))
                 (:lab05 (get-lab-files "Lab05"))
                 (:lab06 (get-lab-files "Lab06"))
                 (:lab07 (get-lab-files "Lab07"))
                 (:lab08 (get-lab-files "Lab08"))
                 (:lab09 (get-lab-files "Lab09"))
                 (:exam (directory test-cases-folder))
                 (otherwise (error "Invalid lab identifier ~a.~%Lab identifiers are in the form :labXX, where XX is the lab number, e.g., :lab03." lab)))))
         (solution-evaluations (grade-solutions solution-files test-cases-files)))
    (generate-messages out (list (/ (reduce #'+ solution-evaluations :key #'caadr)
                                  (length test-cases-files))
                               solution-evaluations)))
  (in-package :cl-user))


(in-package :cg)

(defun start ()
  
  (format t
          "   
                       <<  Welcome to CodeGrader  >>

   To grade students' solutions, use the GRADE-IT function as describe at

                   https://github.com/marcus3santos/codegrader

   NOTE: once you launch function GRADE-IT, it will start evaluating the students'
   solutions and you may see on your REPL  error/warning messages and output
   generated by the student's solution.

   To go back to CL-USER, type: (quit)")
  (in-package :cg))

(defun quit ()<
  (in-package :cl-user))

