(in-package :grader)

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
#|
(defun contains-forbidden-symbol? (asked-function prog-file forbidden-symbols)
  (let ((chked-forbidden-symbols (mapcar #'(lambda (s) (if (stringp s) (intern s) s)) forbidden-symbols))
        (student-solution (with-open-file (stream prog-file :direction :input)
                            (loop for form = (read stream nil nil)
                                  while form
                                  collect form))))
    (used-forbidden-function-p asked-function chked-forbidden-symbols student-solution))
|#

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

(defun read-file-as-string (file-name)
  "Reads the content of the file specified by FILE-NAME and returns it as a string."
  (with-open-file (stream file-name :direction :input)
    (let ((content (make-string-output-stream))) ;; Create a stream to collect the file content
      (loop for line = (read-line stream nil nil)
            while line
            do (format content "~A~%" line)) ;; Append each line to content with a newline
      (get-output-stream-string content))))

(defun run-testcases (question)
  (let ((cur *package*))
    (in-package :test-runtime)
    (setf *results* nil)
    (setf *runtime-error* nil)
    (setf *cr-warning* nil)
    (eval `(,(intern (format nil "TEST-~a" (string-upcase question)))))
    (setf *package* cur)))

(defun load-student-solution (student-solution)
  (setf *load-error* nil)
  (setf *load-error-message* (handle-solution-loading student-solution)))

(defun score-result (student-solution question assessment-data ws load-error)
    (let* ((description "")
           (question-data (cdr (assoc question assessment-data :test #'string=)))
           (forb-data (cdr (assoc "forbidden-symbols" question-data :test #'string=)))
           (forbidden-symbols (nth 3 forb-data))
           (penalty-forbidden (nth 1 forb-data))
           (whats-asked (second (assoc "whats-asked" question-data :test #'string=)))
           (asked-functions (second (assoc "asked-functions" question-data :test #'string=)))
           (score (calc-mark *results* ws))
           #|
           (forbid-symb (some #'identity
                              (mapcar #'lambda (asked-function)
                                      (contains-forbidden-symbol? asked-function student-solution forbidden-symbols)
                                      asked-functions)))
           |#
           (forbid-symb (contains-forbidden-symbol? student-solution forbidden-symbols))
           )
      (list
       (if forbid-symb
           (* score (- 1 (/ penalty-forbidden 100)))
           score)
       (cond (*runtime-error* "runtime-error")
	     (*load-error* "load-error")
             (*cr-warning* "cr-warning")
             (forbid-symb (list "used forbidden symbol" forbid-symb penalty-forbidden))
	     (t "No RT-error"))
       (progn
         (when *runtime-error*
           (setf description (format nil "~%Runtime error(s) when evaluating the following expressions:~%~{- ~a~%~}"
                                     (mapcar #'(lambda (ce)
                                                 (format nil "~s~%~a" (second ce) (first ce)))
                                             (reverse *runtime-error*)))))
         (if  *load-error*
              (setf description  (concatenate 'string  description load-error))
              (setf description nil))    
         (when forbid-symb
           (setf description  (concatenate 'string  description (format nil "~%You have used a forbidden symbol, ~a, in your Lisp file !!!~%" forbid-symb))))
         description)
       (change-results-readable *results*)
       (read-file-as-string student-solution)
       whats-asked)))

(defun grade-code (student-solution question assessment-data &optional kind ws)
  "Loads the student-solution file, initializes the test-runtime environment, and invokes
   the testcases for that question, and assesses the results.  Returns the percentage of 
   correct results over total results"
  (let ((load-error (load-student-solution student-solution)))
    (run-testcases question)
    (score-result student-solution question assessment-data ws load-error)))


(defun evaluate-solution (student-solution question assessmt-data &optional kind)
  (cond ((null student-solution)
         (list 0 "no-submitted-file" "No submitted file" nil))
        ((not (equal (pathname-type student-solution) "lisp"))
         (list 0 "not-lisp-file" "Not a lisp file" nil))
        (t (grade-code student-solution question assessmt-data kind))))


