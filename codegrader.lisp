;; manager.lisp

(in-package #:codegrader)

;; The percentage of correct answers a student's solutions must achieve before
;; their simplicity score is taken into account.

(defparameter *correctness-threshold* 100) 

(defparameter *penalty-forbidden* 0.5) ;; Penalty (to be multiplied by total lab mark) for using forbidden functions

(defstruct submission
  std-name
  date
  correctness
  simplicity
  rank
  points)

(defun check-input-files (lf)
  (when lf
    (if (probe-file (car lf))
	(check-input-files (cdr lf))
	(error "Folder/file ~S does not exist." (car lf)))))

(defun generate-std-feedback (key eval feedback-folder)
  (let* ((fname (concatenate 'string (subseq key 0 (1- (length key))) ".txt"))
	 (folder (ensure-directories-exist (concatenate 'string  (namestring feedback-folder) fname))))
    (with-open-file (out folder
			 :direction :output :if-exists :supersede)
      (let ((error-type (second eval))
	    (descr (third eval))
	    (res (fourth eval)))
	(format out "Feedback on your assignment solution")
	(cond ((or (equalp error-type "no-submitted-file")
		   (equalp error-type "not-lisp-file")
		   (equal error-type "late-submission")) (format out "~%~%~A" res))
              ((and (listp error-type) (equal (car error-type) 'used-forbidden-function))
               (format out "~%~%!!! Used forbidden function ~A !!!~%" (cadr error-type))
               (format out "~%Total assignment mark deducted by ~a% for using forbidden function.~%" (* *penalty-forbidden* 100))
               (format out "~%Unit test results:~%~{- ~S~%~}" res))
	      ((equal error-type "No RT-error") (format out "~%~%Unit test results:~%~{- ~S~%~}" res))
	      (t (format out "~%~%~A~%~%Unit test results:~%~{- ~S~%~}" descr res)))))))

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

(defun get-insert-grade (log-file-stream stream csv ht f)
  (let* ((std-name (get-std-name csv))
	 (v (gethash std-name ht)))
    (if v
        (let ((new-mark (change-mark-csv csv (funcall f v))))
          (format log-file-stream "Mark of student ~a changed from ~a to ==> ~a~%" std-name csv new-mark)
          (format stream "~A~%"  new-mark))
	(unless (eq f #'submission-simplicity)
          (format log-file-stream "~S did not submit assignment!~%" std-name)
          (format *standard-output* "~S~%" std-name)))))

(defun generate-marks-spreadsheet (log-file-stream d2l-file folder ht f out-file)
  (with-open-file (in d2l-file :direction :input)
    (with-open-file (out (merge-pathnames folder out-file)
			 :direction :output :if-exists :supersede)
      (format out "~A~%" (read-line in nil))
      (unless (eq f #'submission-simplicity)
        (format *standard-output* "The following students did not submit a lab solution:~%"))
      (loop for line = (read-line in nil)
	    while line do
	      (get-insert-grade log-file-stream out line ht f)))))

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

(defun rank-perf-solutions (ht-perf ht-stds &optional (pts (list 25 18 15 12 10 8 6 4 2 1)) (perf (list)))
  (labels ((set-rank (list i pts)
             (when list
               (let ((item (car list)))
                 (setf (submission-rank item) i)
                 (setf (submission-points item) (car pts))
                 (setf (gethash (submission-std-name item) ht-stds) item)
                 (set-rank (cdr list) (1+ i) (cdr pts))))))
    (maphash (lambda (k v)
               (push v perf))
             ht-perf)
    (set-rank (sort perf '< :key #'submission-simplicity) 1 pts)))


(defun generate-kalos-kagathos-file (root-folder table)
  (with-open-file (test-stream (merge-pathnames root-folder "correctness-and-simplicity.csv") :direction :output :if-exists :supersede)
    (maphash (lambda (key value)
               (format test-stream "~a, ~a, ~a, ~a, ~a~%" key (car (submission-correctness value)) (submission-simplicity value) (submission-rank value) (submission-points value)))
             table)))

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


(defun grade-it (submissions-zipped-file lab-grades-export-file test-cases-file results due-date-time &optional solution-baseline code-simplicity-export-file)
  (check-input-files (list submissions-zipped-file lab-grades-export-file test-cases-file))
  (let* ((is-zipped t)
         (root-folder (check-foldername  (namestring (ensure-directories-exist results :verbose T))))
         (feedback-folder (merge-pathnames "student-feedback/" root-folder))
	 (feedback-zipped (merge-pathnames root-folder "student-feedback.zip"))
	 (unzipped-subs-folder (merge-pathnames "submissions/" root-folder))
	 (subms-folder (progn
                         (cleanup-folder feedback-folder)
                         (cleanup-folder unzipped-subs-folder)
                         (create-folder-ifzipped is-zipped submissions-zipped-file unzipped-subs-folder)))
	 (sfolders (directory (concatenate 'string (namestring subms-folder) "*/")))
         (hperf-solutions (make-hash-table :test 'equal))
	 (h-table (make-hash-table :test 'equal)))
    (with-open-file (log-file-stream (ensure-directories-exist (merge-pathnames "codegrader-history/log.txt" (user-homedir-pathname)))
                                     :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
      (let ((broadcast-stream (make-broadcast-stream *standard-output* log-file-stream)))
        (format broadcast-stream "~a: Started marking assignments~%Solution correctness:~%" (get-date-time))
        (dolist (folder sfolders)
          (multiple-value-bind (key date) (get-key-and-date folder)
            (let* ((pref (consume-until (consume-until key #\-) #\-)) ;(splice-at-char key #\-))
                   (std-name (subseq pref 1 (1- (length pref))))
                   (student-solution (car (directory (merge-pathnames folder "*.*")))) ; gets first file in the directory
                   (sdate (check-dt (form-date-time (replace-char date #\, #\ ))))
                   (ddate (check-dt due-date-time))
                   (temp-eval (evaluate-solution student-solution ddate sdate test-cases-file nil))
                   (simp (handler-case (program-size student-solution solution-baseline)
                           (error (condition)
                             (format broadcast-stream "Error in student lisp code: ~a~%" condition))))
                   (seval (if (car simp) ;; applies penalty for solution that mentioned a forbidden function
                              (progn (setf (car temp-eval) (* (car temp-eval) *penalty-forbidden*))
                                     (setf (cadr temp-eval) (list 'used-forbidden-function (car simp)))
                                     temp-eval)
                              temp-eval))
                   (item (make-submission :std-name std-name
                                          :date sdate
                                          :correctness seval                                                        
                                          :simplicity (cadr simp))))
              (format log-file-stream "Student *~a*,  result:~%~a~%" std-name seval)
              (setf (gethash std-name h-table) item)
              (if (>= (car seval) *correctness-threshold*)
                         (setf (gethash std-name hperf-solutions) item))
              (generate-std-feedback key seval feedback-folder))))
        (in-package :codegrader)
        (format *standard-output* "~%============================================================================~%")
        (format *standard-output* "Slime produced the above messages when loading the students' solution~%")
        (format *standard-output* "============================================================================~%")
        (format broadcast-stream "Done marking students solutions.~%")
        (format broadcast-stream "Ranking the simplicity of perfect solutions...~%")
        (rank-perf-solutions hperf-solutions h-table)
        (format broadcast-stream "Done.~%")
        (format broadcast-stream "Generating the code simplicity scores cvs file...~%")
        (generate-kalos-kagathos-file root-folder h-table)
        (format broadcast-stream "Done.~%")        
        (format broadcast-stream "Generating the zipped feedback folder...~%")
        (zip:zip feedback-zipped feedback-folder :if-exists :supersede)
        (format broadcast-stream "Done.~%")
        (format broadcast-stream "Generating the grades spreadsheet...~%")
        (generate-marks-spreadsheet log-file-stream lab-grades-export-file root-folder h-table #'(lambda (x) (car (submission-correctness x))) "grades.csv")
        (format broadcast-stream "Done.~%")
        (when  code-simplicity-export-file
          (format broadcast-stream "Generating the code simplicity spreadsheet...~%")
          (generate-marks-spreadsheet log-file-stream code-simplicity-export-file root-folder h-table #'submission-simplicity "simplicity-score.csv")
          (format broadcast-stream "Done.~%"))
        (format broadcast-stream "Assignment grading complete!~%" )
        (format *standard-output* "You may now upload to D2L the following grade files stored in your ~a folder :~%" results)
        (format *standard-output* "- grade.csv : contains the lab assignment correctness marks~%- student-feedback.zip : contains the feedback txt files for each student~%- (optional) simplicity-score.csv : contains the code simplicity score.")
        (in-package :cg)
        "(^_^)"))))

(in-package :cg)

(defun start ()
  
  (format t
          "   
                       <<  Welcome to CodeGrader  >>

   To grade students' assignment solutions, use the GRADE-IT function as describe at

                   https://github.com/marcus3santos/codegrader

   NOTE: once you launch function GRADE-IT, it will start evaluating the students'
   solutions and you may see on your REPL  error/warning messages and output
   generated by the student's solution.

   To go back to CL-USER, type: (quit)")
  (in-package :cg))

(defun quit ()
  (in-package :cl-user))

