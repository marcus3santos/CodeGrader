
(in-package #:codegrader)


(defstruct submission
  std-id
  std-fname
  std-lname
  room-pc
  std-name
  date
  evaluation          ; percentage marks per question and explanations
  total-marks) ; total marks, i.e., (sum correctness marks per question)/(Number of questions)

;; Folder in student's home directory storing their solutions

;; (defparameter *std-sub-folder* "pt/")

(defparameter *assessment-data-folder* "~/quicklisp/local-projects/CodeGrader/Assessment-data/")

;; Root folder where the examples' test case files  and the names of the
;; assessment functions are stored.
;; The actual files should be inside the PT1/ or PT2/ folder, as appropriate


;; List of assessments and labs

(defun check-input-files (lf)
  (when lf
    (if (probe-file (car lf))
	(check-input-files (cdr lf))
	(error "Folder/file ~S does not exist." (car lf)))))

(defun clean-symbol-names (e)
  (cond ((keyword-symbol-p e) e)
        ((symbolp e) (intern (symbol-name e)) )
        ((consp e) (cons (clean-symbol-names (car e))  (clean-symbol-names (cdr e))))
        (t e)))

(defun indent-error-msg (str)
  (format nil "~{    ~a~^~%~}" (str->list (normalize-whitespace (remove-substrings '("SANDBOX:" "TEST_RUNTIME::") str)))))

(defun print-to-string (form)
  "prints the form to a string avoiding stack overflow if the form is 
   endlessly recursive"
  (let ((*print-circle* t))
    (format nil "~a" form)))

(defun gen-message (r)
  (let* ((res (first r))
         (rt-error (when (second r)
                     (let* ((error-name (symbol-name (class-name (class-of (second r)))))
                            (error-str  (print-to-string (second r)))
                            (error-message (if (string= error-name "TIMEOUT")
                                               (format nil "~a ~a" error-str "The program is taking too long to complete, possibly due to infinite recursion or an endless loop. 
Please check your logic and consider adding a termination condition.")
                                               error-str)))
                       (format nil "~%  ~a:~%~a" error-name (indent-error-msg error-message)))))         
         (test (nth 3 r))
         (fcall (clean-symbol-names (second test)))
         (ret (clean-symbol-names (eval (third test)))))
    (cond ((string= res "Pass")
           (format nil "Passed: ~s returned ~s" fcall ret))
          ((and (string= res "Fail") rt-error) (format nil "Failed when evaluating ~s.~a" fcall rt-error))
          (t (format nil "Failed: ~s did not return ~s." fcall  ret)))))

(defun generate-messages (out eval)
  (format out "--EVALUATION FEEDBACK--~%~%NOTE:~%- Each question is worth 100 points.~%- Your score in the assessment is the sum of your questions' points divided by the number of questions in the assessment.~%~%")
  (format out "Your total score: ~a (out of 100)~%" (car eval))
  (dolist (question (cadr eval))
    (let* ((q (car question))
           (qeval (cadr question))
           (mark (first qeval))
           (error-type (second qeval))
	   (descr (third qeval))
	   (res (clean-symbol-names (fourth qeval)))
           (std-sol (nth 4 qeval))
           (question-text (nth 5 qeval))
           (unit-test-name (symbol-name (third (car res))))
           (pos (position #\- unit-test-name))
           (func-name (if pos (subseq unit-test-name (1+ pos))
                          unit-test-name)))
      (format out "~%* QUESTION: ~a~%Your score: ~a points (out of 100).~%" (string-upcase q) mark)
      (format out "Description:~%~{~a~%~}End of ~a description." question-text (string-upcase q))
      (format out "~%---------------------------------------------------------------------------")
      (unless (or (equalp error-type "load-error")
                  (equalp error-type "missing-question-file")
                  (equalp error-type "no-submitted-file")
		  (equalp error-type "not-lisp-file")
		  (equal error-type "late-submission"))
        (format out "~%Your Solution:~%~A~%~%End of Your Solution for ~a." std-sol (string-upcase q))
        (format out "~%---------------------------------------------------------------------------"))
      (when (and  *load-error-message* (not (string= *load-error-message* "")))
        (format out "~%Compile time messages:~%~a~%End of compile time messages for ~a." *load-error-message* (string-upcase q))
        (format out "~%---------------------------------------------------------------------------"))
      (cond ((or (equalp error-type "load-error")
                 (equalp error-type "missing-question-file")
                 (equalp error-type "no-submitted-file")
		 (equalp error-type "not-lisp-file")
		 (equal error-type "late-submission")) 
             (format out "~%Your mark for all parts of this question is zero. ~A !!!" descr))
            ((and (listp error-type) (string= (car error-type) "used forbidden symbol"))
             (format out "~%!!! You have used a forbidden symbol, ~A, in your Lisp file !!!~%" (cadr error-type))
             (format out "~%Your mark for all parts of this question was reduced by ~a% for using forbidden symbol.~%" (caddr error-type))
             ;;(format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}" func-name (mapcar #'gen-message res))
             )
            #|
            ((equal error-type "No RT-error") 
             (format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}" func-name (mapcar #'gen-message res)))
	    (t (format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}" func-name (mapcar #'gen-message res)))
            |#
            )
      (format out "~%Unit Test Results - function ~a:~%~{- ~a~%~}End of Unit Tests for ~a." func-name (mapcar #'gen-message res) (string-upcase q))
      (format out "~%---------------------------------------------------------------------------")))
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


(defun grade-solutions (solution-files question-names assessment-data)
  "From a list of solution files' paths and a list containing the folder name and
   the list of question names, e.g., q1, q2, ..., returns a list containing the results
   of the solutions evaluatios"
  (let (results
        (sol-fnames (mapcar #'file-namestring solution-files)))
    (dolist (question-name question-names)
      (let ((qfname (format nil "~a.lisp" question-name)))
        (push (list question-name
                    (if (member qfname sol-fnames :test #'string=)
                        (let* ((solution (get-solution qfname solution-files))
                               (evaluation (evaluate-solution solution question-name assessment-data "hidden")))
                          (if (and (stringp (second evaluation)) (string= (second evaluation) "load-error"))
                              (list 0 "load-error" "Your program contains unbalanced parentheses and cannot be compiled. Please check for missing or extra parentheses in the source file." nil)
                              evaluation))
                        (list 0 "missing-question-file" (format nil "~a file not found" qfname))))
              results)))
    (reverse results)))
#|
(defun grade-solutions (solution-files test-cases-files)
  (let ((results (list))
        (sol-fnames (mapcar #'file-namestring solution-files)))
    (dolist (test-case test-cases-files)
      (push (list (pathname-name (file-namestring test-case))
                  (if (member (file-namestring test-case) sol-fnames :test #'string=)
                      (let* ((solution (get-solution (file-namestring test-case) solution-files))
                             (evaluation (evaluate-solution solution "hidden" test-case)))
                        (if (string= (second evaluation) "load-error")
                            (list 0 "load-error" "Your program contains unbalanced parentheses and cannot be compiled. Please check for missing or extra parentheses in the source file." nil)
                            evaluation))
                      (list 0 "missing-question-file" (concatenate 'string (file-namestring test-case) " file not found" nil))))
            results))
    (reverse results)))
|#

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


(defun load-questions-testcases (assessment-data questions test-cases-kind)
  (let ((current *package*))
    (in-package :test-runtime)
    (dolist (question questions)
      (let* ((question-data (cdr (assoc question assessment-data :test #'string=)))
             (testcase-code (cdr (assoc test-cases-kind question-data :test #'string=))))
        (if testcase-code
            (load-test-cases testcase-code)
            (error "Missing hidden test cases for question ~a. ~%Ensure you have generated the assessment data file by setting the :hidden key to T, as follows: ~%(GEN-EXAM-FILES <sxm file> :HIDDEN T)" question))))
    (setf *package* current)))


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


;;(in-package :cg)

(defun start ()
  
  (format t
          "   
                       <<  Welcome to CodeGrader  >>

   To grade students' solutions, use the GRADE-EXAM function as described at

                   https://github.com/marcus3santos/codegrader

   NOTE: once you launch GRADE-EXAM, it will start evaluating the students'
   solutions and you may see on your REPL  error/warning messages and output
   generated by the student's solution.

   To go back to CL-USER, type: (quit)")
  (in-package :cg))

(defun quit ()
  (in-package :cl-user))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grade-exam (submissions-zipped-file std-pc-map assessment-tooling-file results-folder &optional exam-grades-export-file)
  "Main function to grade an exam.
   submissions-zipped-file: Zipped file containing student solutions.
   std-pc-map: CSV file with student ID, name, and room-machine ID.
   assessment-tooling-file: Tooling file for the assessment.
   results-folder: Folder where CodeGrader will save results.
   exam-grades-export-file: Optional file to export exam grades."
  (check-input-files (append (when exam-grades-export-file (list exam-grades-export-file))
                             (list submissions-zipped-file std-pc-map assessment-tooling-file)))

  (let* ((results-folder (setup-results-folder results-folder))
         (assessment-data (load-and-process-assessment-data assessment-tooling-file))
         (xport-funcs (second (assoc "fnames" (car assessment-data) :test #'string=)))
         (assessment-questions (second (assoc "questions" (car assessment-data) :test #'string=)))
         (feedback-folder (merge-pathnames "student-feedback/" results-folder))
         (subs-folder (merge-pathnames "submissions/" results-folder))
         (map (create-mapping-table std-pc-map)))

    (prepare-submission-environment subs-folder submissions-zipped-file feedback-folder)
    (load-questions-testcases (cdr assessment-data) assessment-questions "hidden")
    (with-open-file (log-file-stream (ensure-directories-exist (merge-pathnames "codegrader-history/log.txt" (user-homedir-pathname)))
                                     :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
      (let ((broadcast-stream (make-broadcast-stream *standard-output* log-file-stream)))
        (format broadcast-stream "~a: Started marking~%" (get-date-time))
        (process-student-submissions (directory (concatenate 'string (namestring subs-folder) "*/"))
                                     map
                                     assessment-questions
                                     (cdr assessment-data)
                                     feedback-folder
                                     log-file-stream) ; Passed here
        (finalize-grading broadcast-stream subs-folder exam-grades-export-file results-folder map log-file-stream) ; Added log-file-stream here
        "(^_^)"))))


(defun setup-results-folder (folder-path)
  "Ensures the results folder exists and returns its namestring."
  (check-foldername (namestring (ensure-directories-exist folder-path :verbose T))))

(defun load-and-process-assessment-data (assessment-tooling-file)
  "Loads assessment data, exposes functions, and subst-packages symbols.
   Returns a cons cell: (original-data . processed-data)"
  (let* ((assessment-data-orig (with-open-file (in assessment-tooling-file :direction :input)
                                 (read in)))
         (xport-funcs (second (assoc "fnames" assessment-data-orig :test #'string=)))
         (assessment-data-processed (progn
                                      (export (mapcar (lambda (s) (intern (symbol-name s) :sandbox)) xport-funcs) :sandbox)
                                      (subst-package-symbols assessment-data-orig :test-runtime xport-funcs :sandbox))))
    (cons assessment-data-orig assessment-data-processed)))

(defun prepare-submission-environment (subs-folder submissions-zipped-file feedback-folder)
  "Cleans up folders and unzips submissions."
  (cleanup-folder feedback-folder)
  (cleanup-folder subs-folder)
  (uiop:run-program (concatenate 'string "unzip " (namestring submissions-zipped-file) " -d " (namestring subs-folder))))

(defun process-student-submissions (sfolders map assessment-questions assessment-data feedback-folder log-file-stream)
  "Iterates through student submission folders and grades each student's work."
  (dolist (folder sfolders)
    (let* ((str (namestring folder))
           (temp (subseq str (1+ (position #\/ (subseq str 0 (1- (length str))) :from-end t))))
           (room-pc (subseq temp 0 (1- (length temp))))
           (std (gethash room-pc map)))
      (when std
        (grade-single-student folder std assessment-questions assessment-data feedback-folder map log-file-stream)
        (format t "~%~a~VT~C -- Graded" (cdr std) 40 #\tab)))))

(defun last-folder (path)
  "Return the last folder name from PATH, followed by a slash."
  (let* ((clean-path (if (char= (char path (1- (length path))) #\/)
                         (subseq path 0 (1- (length path)))
                         path))
         (last-slash (position #\/ clean-path :from-end t))
         (folder (subseq clean-path (if last-slash (1+ last-slash) 0))))
    (concatenate 'string folder "/")))

(defun grade-single-student (folder std assessment-questions assessment-data feedback-folder map log-file-stream)
  "Grades a single student's submission."
  (let* ((std-sub-folder (concatenate 'string (car (last (pathname-directory (second (assoc "folder" assessment-data :test #'string=))))) "/"))
         (student-files (directory (concatenate 'string (namestring  folder) std-sub-folder "*.*")))
         (solutions-evaluations (grade-solutions student-files assessment-questions assessment-data))
         (seval (list (/ (reduce #'+ solutions-evaluations :key #'caadr) (length assessment-questions))
                      solutions-evaluations))
         (item (make-submission :std-id (first std)
                                :std-fname (second std)
                                :std-lname (third std)
                                :room-pc (fourth std)
                                :evaluation seval
                                :total-marks (car seval)))
         (anony-id (format nil "~A" (sxhash (submission-std-id item)))))
    (format log-file-stream "Student ~a (~a ~a),  result:~%~s~%" (submission-std-id item) folder (concatenate 'string anony-id ".txt") seval)
    (setf (gethash (submission-std-id item) map) item)
    (generate-feedback anony-id seval feedback-folder)))

(defun finalize-grading (broadcast-stream subs-folder exam-grades-export-file results-folder map log-file-stream)
  "Performs final actions after all students are graded."
  (in-package :codegrader)
  (format broadcast-stream "~%~%Done marking students solutions.~%")
  (when exam-grades-export-file (format broadcast-stream "Generating the grades spreadsheet...~%"))
  (generate-exam-marks-spreadsheet log-file-stream exam-grades-export-file results-folder map #'(lambda (x) (submission-total-marks x)) "grades.csv")
  (when exam-grades-export-file (format broadcast-stream "Done.~%"))
  (sb-ext:delete-directory (namestring subs-folder) :recursive t)
  (format broadcast-stream "Exam grading complete!~%" )
  (format *standard-output* "You may now upload to D2L the following grade files stored in your ~a folder :~%" results-folder)
  (when exam-grades-export-file
    (format *standard-output* "- grade.csv : contains the test marks~%"))
  (format *standard-output* "- student-feedback/ : contains the feedback txt files for each student.")
  (in-package :cl-user))

(defun probe-assessment-file (file)
  (let* ((pnd (pathname-directory file))
         (home (nth (- (length pnd) 2) pnd))
         (user-name (car (last (pathname-directory (user-homedir-pathname))))))
    (and (or (and (symbolp home)
                  (string= "HOME" (symbol-name home)))
             (and (stringp home)
                  (string= user-name home)))
         (probe-file file))))

(defun chk-my-solution (a#)
  "Checks a student's solution file against examples.
   a#: String identifying the solution file, e.g., '~/lab01/q1.lisp'."
  (unless (probe-assessment-file a#)
    (error "~%!!!!!!!! Error: You saved your file in the wrong folder. Please save it in the specified folder. !!!!!!!!"))
  (let* ((assessment-data-file (derive-assessment-data-file a#))
         (assessment-data (load-and-process-assessment-data-for-chk-my-solution assessment-data-file))
         (question-name (pathname-name a#))
         (current-pckg *package*))
    (unwind-protect
         (let* ((eval (load-and-evaluate-solution a# question-name assessment-data))
                (error-type (second eval)))
           (handle-evaluation-output error-type eval question-name))
      (setf *package* current-pckg)))
  t)

(defun derive-assessment-data-file (solution-file-path)
  "Derives the assessment data file path from the solution file path."
  (let* ((assessment-folder-name (car (last (pathname-directory solution-file-path))))
         (assessment-data-file (format nil "~a~a.data" *assessment-data-folder* assessment-folder-name)))
    assessment-data-file))

(defun load-and-process-assessment-data-for-chk-my-solution (assessment-data-file)
  "Loads and processes assessment data for chk-my-solution."
  (let* ((assessment-data-orig (handler-case (with-open-file (in assessment-data-file :direction :input)
                                               (read in))
                                 (file-error (e) (error "!!!!! Incorrect folder/file name.   !!!!!" e))))
         (sandbox-functions (second (assoc "fnames" assessment-data-orig :test #'string=))))
    (export (mapcar (lambda (s) (intern (symbol-name s) :sandbox)) sandbox-functions) :sandbox)
    (subst-package-symbols assessment-data-orig :test-runtime sandbox-functions :sandbox)))

(defun load-and-evaluate-solution (folder-file question-name assessment-data)
  "Loads questions and evaluates the solution."
  (load-questions-testcases assessment-data (list question-name) "given")
  (evaluate-solution folder-file question-name assessment-data "given"))

(defun handle-evaluation-output (error-type eval question-name)
  "Handles and formats the output of the solution evaluation."
  (when (and (listp error-type) (string= (car error-type) "used forbidden symbol"))
    (format t "~%!!! You have used a forbidden symbol, ~A, in your Lisp file !!!~%" (cadr error-type))
    (format t "~%Your mark for all parts of this question will be reduced by ~a% for using a forbidden symbol.~%" (caddr error-type)))
  (cond ((and (stringp error-type) (string= error-type "runtime-error"))
         (format t "~a" (nth 2 eval)))
        ((and (stringp error-type) (string= error-type "load-error"))
         (format t "~%~a" *load-error-message*))
        (t
         (when (and  *load-error-message* (not (string= *load-error-message* "")))
           (format t "~%---------------------------------------------------------------------------")
           (format t "~%Compile time messages:~%~a" *load-error-message*))
         (format t "~%---------------------------------------------------------------------------")
         (format t "~%When testing your solution for ~A, the results obtained were the following:~%~{- ~a~%~}" question-name (mapcar #'gen-message (nth 3 eval))))))

;;

(defun test1 ()
  (let ((current *package*))
    (in-package :test-runtime)
    (multiple-value-bind (lambda-form name env)
        (function-lambda-expression #'test-runtime::test-double-positives)
      (print lambda-form))
    (setf *package* current)))
