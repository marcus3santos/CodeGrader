;; comp-exam.lisp

(defparameter *parent-folder* "Gen-files/")

(defparameter *question-marker* "** Question ")

(defparameter *exercise-marker* "** Exercise ")

(defparameter *preamble-marker* "*** Preamble")

(defparameter *wyaa-marker* "*** What you are asked")

(defparameter *title-marker* "#+TITLE:")

(defparameter *folder-marker* "#+FOLDER:")

(defparameter *begin-examples-marker* "#+BEGIN_EXAMPLE")

(defparameter *end-examples-marker* "#+END_EXAMPLE")

(defparameter *begin-test-cases-maker* "#+BEGIN_TCS")

(defparameter *end-test-cases-maker* "#+END_TCS")

(defparameter *REPL-marker* "CL-USER>")

(defparameter *dangerous-functions* '("OPEN" "LOAD" "EVAL" "DELETE-FILE" 
   "WITH-OPEN-FILE" "RUN-PROGRAM" "SB-EXT:RUN-PROGRAM"
   "PROBE-FILE" "FILE-WRITE-DATE" "RENAME-FILE" "ENSURE-DIRECTORIES-EXIST"
   "DIRECTORY" "READ" "WRITE" "READ-LINE" "READ-FROM-STRING"
   "COMPILE" "COMPILE-FILE" "QUIT" "GC" "FORMAT"
   "DEFPACKAGE" "IN-PACKAGE"))

(defstruct question
  number forbidden penalty wyaa examples test-cases)

(defun sect-marker? (line str)
  (let ((nhline (trim-head-spcs line)))
    (when (and (coerce nhline 'list) (>= (length nhline) (length str)))
      (let ((tkn (subseq (string-upcase nhline) 0 (length str))))
        (if (equal (string-upcase str) tkn)
            line)))))

(defun get-params (line)
  (let ((params (read-from-string line nil nil)))
    (if params params
        (error "Missing question parameters!"))))


(defun emit (out str)
  (format out "~a~%" str)
  str)

(defun emit-code (out form)
  (pprint form out))

(defun read-objects-from-string (input-string)
  "Reads all Lisp objects from INPUT-STRING and returns them as a list."
  (let ((result '())                  ; List to store objects
        (position 0))                 ; Current position in the string
    (loop
      (multiple-value-bind (object new-position)
          (ignore-errors (read-from-string input-string nil nil :start position))
        (if object
            (progn
              (push object result) ; Add the object to the result list
              (setf position new-position)) ; Update position for next read
            (return))))           ; Exit the loop when no more objects
    (nreverse result)))

(defun cr-pairs (a)
  (if (null a) a
      (cons (list (car a) (cadr a)) (cr-pairs (cddr a)))))

(defun gen-example (out examples)
  (dolist (pair (cr-pairs (cddr examples)))
    (emit out (format nil "~a ~s" *REPL-marker* (car pair)))
    (emit out (if (and (listp (cadr pair)) (eql (car (cadr pair)) 'quote))
                  (format nil "(~{~s~^ ~})" (second (cadr pair)))
                  (format nil "~s" (cadr pair))))))

(defun trim-head-spcs (str)
  (subseq str (or (position #\Space str :test-not #'char=) 0)))

(defun trim-tail-spcs (str)
  (if (zerop (length str)) str
      (subseq str 0 (1+ (position #\Space  str :test-not #'char= :from-end t)))))

(defun comp-exam (from ht)
  (if (probe-file from)
      (with-open-file (in from)
        (let ((to (ensure-directories-exist
		   (concatenate 'string (directory-namestring from) *parent-folder* (format nil "~a-description.org" (pathname-name (file-namestring from)))))))
	  (with-open-file (out to :direction :output
				  :if-exists :supersede)
            (emit out "#+Options: toc:nil num:nil date:nil author:nil")
            (let ((title-flag nil)
		  (folder-flag nil)
                  (question-flag nil)
                  (preamble-flag nil)
                  (examples-flag nil)
                  (test-cases-flag nil)
                  (qtext nil)
                  (examples nil)
                  (test-cases nil))
	      (loop for line = (read-line in nil nil)
		    while line do
		      ;;(format t "~a" (aref #(#\/ #\\) (random 2)))
                      (format t "Read: ~a~%" line)
		      (cond
			((sect-marker? line *title-marker*)  ;; Title
                         (setf title-flag t)
                         (emit out line))
                        ((sect-marker? line *folder-marker*)   ;; Folder
                         (let ((folder (subseq line (length *folder-marker*))))
                           (setf folder-flag (trim-head-spcs (trim-tail-spcs folder)))))
                        ((or (sect-marker? line *question-marker*)     ;; Question/Exercise
                             (sect-marker? line *exercise-marker*))
			 (unless title-flag
                           (error "Missing ~a from org file header" *title-marker*))
                         (unless folder-flag
                           (error "Missing #+FOLDER from org file header"))
                         (let* ((params (read-objects-from-string (subseq line (length *question-marker*))))
                                (number (first params))
                                (penalty (nth 2 params))
                                (forbidden (nth 4 params)))
                           (unless (numberp number)
                             (error "Missing question number!"))
                           (when question-flag
                             (setf (question-wyaa (gethash question-flag ht)) (remove "" qtext :test #'string=)
                                   qtext nil))
                           (setf question-flag number
                                 (gethash question-flag ht) (make-question :number number
                                                                           :penalty (when forbidden (/ penalty 100.0))
                                                                           :forbidden forbidden)
                                 examples nil))
                         (emit out (format nil "~a~a" *question-marker* question-flag)))
                        ((sect-marker? line *preamble-marker*)  ;; In question preamble
                         (setf preamble-flag t)
                         (emit out *preamble-marker*))
                        ((sect-marker? line *wyaa-marker*)  ;; In the What You are Asked section
                         (setf preamble-flag nil)
                         (push "*WHAT YOU ARE ASKED*:" qtext)
                         (emit out "*** WHAT YOU ARE ASKED")
                         (push (emit out "") qtext)                         
                         (push (emit out "*NOTE*:") qtext)
                         (push (emit out (format nil "- You are required to write the solutions for the parts of this question in the Lisp program file *~a/q~a.lisp*" folder-flag question-flag)) qtext)
                         (push (emit out "- You may create helper functions in your program file.") qtext)
                         (push (if (question-penalty (gethash question-flag ht))
                                   (emit out (format nil "- You must not use or refer to the following Lisp built-in function(s) and symbol(s): ~{~a~^, ~}. The penalty for doing so is a deduction of ~a% on the score of your solutions for this question." (question-forbidden (gethash question-flag ht)) (* 100 (question-penalty (gethash question-flag ht)))))
                                   (emit out "- There are no restrictions in the use of Lisp built-in functions or symbols in the parts of this question."))
                               qtext))
                        ((sect-marker? line *begin-examples-marker*) ;; Examples begin
                         (unless preamble-flag
                           (setf examples-flag t))
                         (emit out *begin-examples-marker*))
                        ((sect-marker? line *end-examples-marker*) ;; Examples end
                         (unless preamble-flag
                           (setf examples-flag nil)
                           (push (car (read-objects-from-string (apply #'concatenate 'string (reverse examples))))
                                 (question-examples (gethash question-flag ht)))
                           (gen-example out (car (read-objects-from-string (apply #'concatenate 'string (reverse examples)))))
                           (setf examples nil))
                         (emit out *end-examples-marker*))
                        ((and (not preamble-flag) examples-flag) (push line examples))  ;; Inside an example block
                        ((sect-marker? line *begin-test-cases-maker*) ;; Test cases begin
                         (unless preamble-flag
                           (setf test-cases-flag t)))
                        ((sect-marker? line *end-test-cases-maker*) ;; Test cases end
                         (unless preamble-flag
                           (setf test-cases-flag nil)
                           (push (car (read-objects-from-string (apply #'concatenate 'string (reverse test-cases))))
                                 (question-test-cases (gethash question-flag ht)))
                           (setf test-cases nil)))
                        ((and (not preamble-flag) test-cases-flag) (push line test-cases))  ; Inside a test case block
                        (t (if (and question-flag (not preamble-flag))
                               (push (emit out line) qtext)
                               (emit out line)))))
              (when question-flag
                (setf (question-wyaa (gethash question-flag ht)) qtext))))))))



(defun gen-cases (out macro-name examples)
  (emit-code out
               `(deftest ,macro-name ()
                  (check
                    ,@(let ((res))
                        (dolist (e examples (reverse res)) 
                          (push `(equalp ,(car e) ,(cadr e)) res))))))
  (format out "~%"))

(defun gen-tcs (from qlabel wyaa forbidden penalty examples &optional folder-name)
  (let ((to (ensure-directories-exist
	     (concatenate 'string (directory-namestring from) *parent-folder* folder-name qlabel ".lisp"))))
    (with-open-file (out to :direction :output :if-exists :supersede)
      (emit-code out `(whats-asked (quote ,wyaa)))
      (emit out "")
      (emit-code out `(forbidden-symbols :penalty ,penalty :symbols (quote ,forbidden)))
      (emit out "")
      (let ((fm-names))
        (dolist (g-examples  examples)
          (let ((fm-name-cases (list (second g-examples) (intern (format nil "TEST-~a" (second g-examples))) (cr-pairs (cddr g-examples)))))
            (push fm-name-cases fm-names)
            (gen-cases out (second fm-name-cases) (third fm-name-cases))))
        (emit-code out
                   `(defun ,(intern (format nil "TEST-~a" (string-upcase qlabel))) ()
                      ,@(let ((res)
                              (rfm-names (reverse fm-names)))
                          (dolist (e rfm-names)
                            (push (list (second e)) res))
                          (dolist (e rfm-names (reverse res))
                            (push (list 'fmakunbound  (list 'quote (first e))) res)))))
        (emit out "")
        (emit-code out `(,(intern (format nil "TEST-~a" (string-upcase qlabel)))))))))


(defun gen-exam-files (exam-specs)
  (let ((ht (make-hash-table))
        (qlabels (list))
        (all-fnames (list))
        (to (ensure-directories-exist
	     (concatenate 'string (directory-namestring exam-specs) *parent-folder* "Assessment-functions/assessment-functions.lisp"))))
    (comp-exam exam-specs ht)
    (maphash (lambda (k v)
               (declare (ignore k))
               (let* ((qlabel (format nil "q~a" (question-number v)))
                      (forbidden (question-forbidden v))
                      (penalty (question-penalty v))
                      (wyaa (reverse (question-wyaa v)))
                      (examples (reverse (question-examples v)))
                      (test-cases (reverse (question-test-cases v)))
                      (fnames (mapcar #'second examples)))
                 (push (string-upcase qlabel) qlabels)
                 (setf all-fnames (append fnames all-fnames))
                 (gen-tcs exam-specs qlabel wyaa forbidden penalty examples "Examples/")
                 (gen-tcs exam-specs qlabel wyaa forbidden penalty test-cases "Test-Cases/")
                 (format t "~%~a:~%Forbidden: ~a~%Penalty: ~a~%Question: ~a~%Examples: ~a~%TCs: ~a~%" qlabel forbidden penalty wyaa examples test-cases
                         ))) ht)
    (with-open-file (out to :direction :output :if-exists :supersede)
      (format out "~a" all-fnames))))
