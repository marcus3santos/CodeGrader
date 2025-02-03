(in-package :grader)

(defparameter *system-name* :codegrader)

(defvar *inside-multiline-comment* nil)


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


(defun read-lisp-file (file-path)
  (setf *inside-multiline-comment* nil)
  (with-open-file (stream file-path)
    (loop for line = (read-line stream nil)
          while line
          collect (remove-comments line))))



(defun remove-comments (line)
  (let* ((semicolon-position (position #\; line))
         (multiline-start (search "#|" line))
         (multiline-end (search "|#" line)))
    (cond
      ;; Single-line comment
      ((and (not *inside-multiline-comment*)
            semicolon-position
            (or (not multiline-start) (< semicolon-position multiline-start)))
       (subseq line 0 semicolon-position))
    ;; Multiline comment start
    
      (multiline-start
       (if (and (not *inside-multiline-comment*)
                multiline-end (> multiline-end multiline-start))
           (let ((before-multiline (subseq line 0 multiline-start))
                 (after-multiline (subseq line (+ 2 multiline-end))))
             (concatenate 'string before-multiline after-multiline))
           (progn
             (setf *inside-multiline-comment* t)
             (subseq line 0 multiline-start))))
      ;; Multiline comment end
      ((and *inside-multiline-comment* multiline-end (not multiline-start))
       (setf *inside-multiline-comment* nil)
       (subseq line (+ 2 multiline-end)))
      (*inside-multiline-comment* "")
      ;; No comment found
      (t line))))

(defun extract-symbols-from-file (file)
  (let ((file-string (apply #'concatenate 'string (read-lisp-file file))))
    (with-input-from-string (stream file-string)
      (let ((symbols '())
            (current-symbol '())
            (inside-symbol nil))
        (labels ((process-char (char)
                   (cond
                     ((or (char= char #\() (char= char #\)) (char= char #\Space) (char= char #\Newline) (char= char #\Tab))
                      (when inside-symbol
                        (push (coerce (reverse current-symbol) 'string) symbols)
                        (setf current-symbol '())
                        (setf inside-symbol nil)))
                     (t
                      (setf current-symbol (cons char current-symbol))
                      (setf inside-symbol t)))))
          (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
              ((eq char :eof) (return symbols))
            (process-char char)))))))


(defun contains-forbidden-symbol? (prg-file frbn-symbs)
  (let ((fsymbs (if (stringp (car frbn-symbs))
                    (mapcar #'string-upcase frbn-symbs)
                    (mapcar #'symbol-name frbn-symbs)))
        (cap-symbs (mapcar #'string-upcase (extract-symbols-from-file prg-file))))
    (labels ((check-fnames (e)
               (cond ((null e) nil)
                     ((member (car e) fsymbs :test #'equal) (car e))
                     (t (check-fnames (cdr e))))))
      (check-fnames cap-symbs))))

#|
(defun contains-forbidden-symbol? (prg-file)
  ;;(setf *forbidden-symbols* nil)
  (let ((ffuncs (chng-to-string *forbidden-symbols*))
        (cap-symbs (capitalize-list (extract-symbols-from-file prg-file))))
    (labels ((check-fnames (e)
               (cond ((null e) nil)
                     ((member (car e) ffuncs :test #'equal) (car e))
                     (t (check-fnames (cdr e))))))
      (check-fnames cap-symbs))))
|#

(defun read-file-as-string (file-name)
  "Reads the content of the file specified by FILE-NAME and returns it as a string."
  (with-open-file (stream file-name :direction :input)
    (let ((content (make-string-output-stream))) ;; Create a stream to collect the file content
      (loop for line = (read-line stream nil nil)
            while line
            do (format content "~A~%" line)) ;; Append each line to content with a newline
      (get-output-stream-string content))))

(defun grade-code (student-solution test-cases &optional ws)
  "Loads the student-solution file, loads the test cases, runs
  the test cases, and returns the percentage of correct results over total results"
  (let ((description "No runtime errors"))
    (handle-solution-loading student-solution)
    (in-package :test-runtime)
    (setf *results* nil)
    (setf *runtime-error* nil)
    (setf *load-error* nil)
    (setf *cr-warning* nil)
    (setf *forbidden-symbols* nil)
    (load-macros)
    (load test-cases)
    (in-package :grader)
    (let ((score (calc-mark *results* ws))
          (forbid-symb (contains-forbidden-symbol? student-solution *forbidden-symbols*))
          (error-types nil))
      (list
       (if forbid-symb
           (* score (- 1 *penalty-forbidden*))
           score)
       (cond (*runtime-error* "runtime-error")
	     (*load-error* "load-error")
             (*cr-warning* "cr-warning")
             (forbid-symb (list "used forbidden symbol" forbid-symb *penalty-forbidden*))
	     (t "No RT-error"))
       (progn
         (if *runtime-error* 
             (setf description (format nil "Runtime error(s) when evaluating the following expressions:~%~{- ~A~%~}" (reverse *runtime-error*)))
             (setf description "No runtime errors."))
         (when *load-error*
           (setf description  (concatenate 'string  description "Load/Compiling error.")))    
         (when *cr-warning*
           (setf description  (concatenate 'string  description "CR character warning! Student's lisp file contains a CR character. New temporary file generated, loaded, and deleted.")))    
         (when forbid-symb
           (setf description  (concatenate 'string  description (format nil "~%You have used a forbidden symbol, ~a, in your Lisp file !!!~%" forbid-symb))))
         description)
       (change-results-readable *results*)
       (read-file-as-string student-solution)))))


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

