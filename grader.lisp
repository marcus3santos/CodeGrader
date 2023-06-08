(in-package :grader)

(defparameter *system-name* :codegrader)


(defun month->number (name)
  (let* ((sname (symbol-name name))
         (alist '(("Jan" . 1)
                  ("Feb" . 2)
                  ("Mar" . 3)
                  ("Apr" . 4)
                  ("May" . 5)
                  ("Jun" . 6)
                  ("Jul" . 7)
                  ("Aug" . 8)
                  ("Sep" . 9)
                  ("Oct" . 10)
                  ("Nov" . 11)
                  ("Dec" . 12)))
         (n (cdr (assoc sname alist :test #'equalp))))
    (if n n
        (error "You have provided an incorrect month abbreviation: ~a" name))))

(defun month->seconds (m)
  (let ((mn (month->number m)))
    (if (and (>= mn 1) (<= mn 12))
        (* mn 30 24 60 60)
        (error "You have provided an invalid month: ~a" m))))


(defun day->sec (d)
  (if (and (>= d 1) (<= d 31))
      (* d 24 60 60)
      (error "You have provided an invalid day: ~a" d)))

(defun date->seconds (date)
  (+ (month->seconds (car date)) (day->sec (cadr date))))

(defun conv-ampm (ampm)
  (let ((sampm (symbol-name ampm)))
    (cond ((equalp sampm "am") 'am)
          ((equalp sampm "pm") 'pm)
          (t 'x))))

(defun time-ampm->time-24hrs (time)
  (let ((h (car time))
	(p (conv-ampm  (cadr time))))
    (if (and (or (eq p 'am) (eq p 'pm))
             (>= h 0)
             (<= h 1259))
        (if (or (eq p 'am) 
                (and (eq p 'pm) (>= h 1200) (<= h 1259)))
            h
            (+ h 1200))
        (error "You have provided an invalid time: ~a" time))))
	  
(defun >time (tp1 tp2)
  (let ((t1 (time-ampm->time-24hrs tp1))
	(t2 (time-ampm->time-24hrs tp2)))
    (> t1 t2)))

(defun >date (dt1 dt2)
  "True if datetime d1 is greater than d2. Both are in 
the format (Nov 5 102 AM)"
  (let* ((date1 (list (car dt1) (cadr dt1)))
	 (time1 (cddr dt1))
	 (date2 (list (car dt2) (cadr dt2)))
	 (time2 (cddr dt2))
	 (datesec1 (date->seconds date1))
	 (datesec2 (date->seconds date2)))
    (or (> datesec1 datesec2)
	(and (= datesec1 datesec2)
	     (>time time1 time2)))))

(defun calc-mark (res ws)
  "Calculates student mark based on the results (res) from running the test 
cases and on the weight associated to each function. If ws is nil then
the mark is calculated as the # of passes divided by the total # of cases.
- ws is a list of pairs (<function-name> <weight>) Note: sum of weights must be 100
- res is the list stored in the global variable *results*"
  (unless res
    (error "No test case definitions were provided."))
  (labels ((get-avg (fn res accumPass accumT)
	     (dolist (x res (if (zerop accumT)
				(error "Test function ~S not defined in unit test" fn)
				(/ accumPass accumT)))
	       (when (equal fn (caddr x))
		 (if (car x)
		     (progn (incf accumPass)
			    (incf accumT))
		     (incf accumT))))))
    (if (null ws)
	(loop for r in res
	      when (car r)
		sum 1 into c
	      finally (return (* (/ c (length res)) 100.0)))
	(loop for w in ws 
	      sum (* (cadr w) (get-avg (car w) res 0 0))))))

(defun change-results-readable (results)
  (when results
    (let* ((result (car results))
	   (pass-fail (car result)))
      (cons (cons (if pass-fail "Pass" "Fail") (cdr result))
	    (change-results-readable (cdr results))))))

(defun load-macros ()
  (load (merge-pathnames (asdf:system-source-directory *system-name*) "macros.lisp")))

(defun grade-code (student-solution test-cases &optional ws)
    "Loads the student-solution file, loads the test cases, runs
  the test cases, and returns the percentage of correct results over total results"
  (let ((description "No runtime errors"))
    (setf *results* nil)
    (setf *runtime-error* nil)
    (setf *load-error* nil)
    (setf *cr-warning* nil)
    (setf *forbidden-functions* nil)
    (in-package :test-runtime)
    (handle-solution-loading student-solution)
    (load-macros)
    (load test-cases)
    (in-package :grader)
    (list (calc-mark *results* ws) ;(format nil "~f" (calc-mark *results* ws))
	  (cond (*runtime-error* (setf description "Runtime error")
				 "runtime-error")
		(*load-error* (setf description "Load/Compiling error")
			      "load-error")
                (*cr-warning* (setf description "CR character warning! Student's lisp file contains a CR character. New temporary file generated, loaded, and deleted.")
                              "cr-warning")
		(t "No RT-error"))
          (cond ((or *runtime-error* *load-error*)
                 (setf description (concatenate 'string
                                                description
                                                (format nil "(s) when evaluating the following expressions:~%~{- ~A~%~}" (reverse *runtime-error*)))))
                (*cr-warning* description)
                (t "No runtime errors"))
	  (change-results-readable *results*))))


(defun evaluate-solution (student-solution  test-cases &optional ddate sdate)
  (cond ((null student-solution)
         (list 0 "no-submitted-file" "No submitted file" nil))
        ((not (equal (pathname-type student-solution) "lisp"))
         (list 0 "not-lisp-file" "Not a lisp file" nil))
        ((and ddate sdate (>date sdate ddate))
         (list 0 "late-submission"  (format nil "Late submission. Assignment due on: ~a Submitted on: ~a~%" ddate sdate) nil))
        (t (grade-code student-solution test-cases))))

#|
(defun mark-std-solution (student-solution test-cases-dir &optional (ws nil))
  "Loads the student-solution file, loads the test cases, runs
  the test cases, and returns the percentage of correct results over total results"
  (let ((description "No runtime errors"))
    (setf *results* nil)
    (setf *runtime-error* nil)
    (setf *load-error* nil)
    (setf *cr-warning* nil)
    (setf *forbidden-functions* nil)
    (handle-solution-loading student-solution)
    (load (merge-pathnames (asdf:system-source-directory :automarker) "macros.lisp"))
    (load test-cases-dir)
    (list (calc-mark *results* ws) ;(format nil "~f" (calc-mark *results* ws))
	  (cond (*runtime-error* (setf description "Runtime error")
				 'runtime-error)
		(*load-error* (setf description "Load/Compiling error")
			      'load-error)
                (*cr-warning* (setf description "CR character warning! Student's lisp file contains a CR character. New temporary file generated, loaded, and deleted.")
                              'cr-warning)
		(t "No RT-error"))
          (cond ((or *runtime-error* *load-error*)
                 (setf description (concatenate 'string
                                                description
                                                (format nil "(s) when evaluating the following expressions:~%~{- ~A~%~}" (reverse *runtime-error*)))))
                (*cr-warning* description)
                (t "No runtime errors"))
	  (change-results-readable *results*))))
|#

