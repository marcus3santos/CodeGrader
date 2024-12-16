;; comp-exam.lisp

(defparameter *parent-folder* "Gen-files/")

(defparameter *question-marker* "** Question ")

(defparameter *forbidden-marker* "#+FORBIDDEN:")

(defparameter *begin-examples-marker* "#+BEGIN_EXAMPLE")

(defparameter *end-examples-marker* "#+END_EXAMPLE")

(defparameter *begin-test-cases-maker* "#+BEGIN_TCS")

(defparameter *end-test-cases-maker* "#+END_TCS")

(defparameter *REPL-marker* "CL-USER>")

(defstruct question
  number forbidden examples test-cases)

(defun sect-marker? (line str)
  (when (and (coerce line 'list) (>= (length line) (length str)))
    (let ((tkn (subseq (string-upcase line) 0 (length str))))
      (if (equal (string-upcase str) tkn) (subseq line (length str))))))

(defun get-qnumber (line)
  (let ((number (read-from-string line nil nil)))
    (if number number
        (error "Missing question number!"))))

(defun get-item (kind input-string)
  "Sequentially read and process items from a string."
  (let ((position 0)
        (length (length input-string)))
    (loop while (< position length)
          do
            (multiple-value-bind (item new-position)
                (read-from-string input-string nil nil :start position)
              (when (null item)
                (return)) ;; Stop processing if we encounter an invalid read
              (when (and (listp item) (string= (string-upcase kind) (string-upcase (format nil "~a" (car item)))))
                (return (cdr item))) 
              (setf position new-position)))))

(defun emit (out str)
  (format out "~a~%" str))

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

(defun gen-example (out examples)
  (dolist (pair (cr-pairs (cddr examples)))
    (emit out (format nil "~a ~a" *REPL-marker* (car pair)))
    (emit out (cadr pair))))

(defun comp-exam (from ht)
  (if (probe-file from)
      (with-open-file (in from)
        (let ((to (ensure-directories-exist
		   (concatenate 'string (directory-namestring from) *parent-folder* (format nil "~a-description.org" (pathname-name (file-namestring from)))))))
	  (with-open-file (out to :direction :output
				  :if-exists :supersede)
            (emit out "#+Options: toc:nil num:nil date:nil author:nil")
            (let ((question-flag nil)
                  (examples-flag nil)
                  (test-cases-flag nil)                  
                  (examples nil)
                  (test-cases nil))
	      (loop for line = (read-line in nil nil)
		    while line do
		      (format t ".")
		      (cond ((sect-marker? line *question-marker*)
                             (setf question-flag (get-qnumber (subseq line (length *question-marker*)))
                                   (gethash question-flag ht) (make-question :number question-flag)
                                   examples nil)
                             (emit out (format nil "~a~a" *question-marker* question-flag)))
                            ((sect-marker? line *forbidden-marker*)
                             (setf (question-forbidden (gethash question-flag ht))
                                   (read-from-string (subseq line (length *forbidden-marker*)) nil nil)))
                            ((sect-marker? line *begin-examples-marker*) ;; Examples begin
                             (setf examples-flag t)
                             (emit out *begin-examples-marker*))
                            ((sect-marker? line *end-examples-marker*) ;; Examples end
                             (setf (question-examples (gethash question-flag ht))
                                   (read-objects-from-string (apply #'concatenate 'string (reverse examples))))
                             (setf examples-flag nil)
                             (gen-example out (car (read-objects-from-string (apply #'concatenate 'string (reverse examples)))))
                             (setf examples nil)
                             (emit out *end-examples-marker*))
                            (examples-flag (push line examples))
                            ((sect-marker? line *begin-test-cases-maker*) ;; Test cases begin
                             (setf test-cases-flag t))
                            ((sect-marker? line *end-test-cases-maker*) ;; Test cases end
                             (setf test-cases-flag nil)
                             (setf (question-test-cases (gethash question-flag ht))
                                   (read-objects-from-string (apply #'concatenate 'string (reverse test-cases))))
                             (setf test-cases nil))
                            (test-cases-flag (push line test-cases))
                            (t (emit out line))))))))))

(defun gen-packages (from qlabel tcs)
  (let ((fnames (mapcar #'(lambda (tc) (second tc)) tcs))
        (to (ensure-directories-exist
		   (concatenate 'string (directory-namestring from) *parent-folder* "Packages/" qlabel ".lisp"))))
    (with-open-file (out to :direction :output :if-exists :supersede)
      (emit-code out
              `(defpackage ,(intern (string-upcase qlabel) :keyword) 
                 (:documentation "Dedicated package for the student's solution store, so it does not polute CodeGrader's name space")
                 (:use cl)
                 (:export ,@fnames))))))

(defun cr-pairs (a)
  (if (null a) a
      (cons (list (car a) (cadr a)) (cr-pairs (cddr a)))))

(defun gen-cases (out macro-name examples)
  (emit-code out
             `(deftest ,macro-name ()
                (check
                  ,@(let ((res))
                      (dolist (e examples (reverse res)) 
                        (push `(equalp ,(car e) ,(cadr e)) res))))))
  (format out "~%"))

(defun gen-tcs (from qlabel forbidden examples &optional folder-name)
  (let ((penalty (car forbidden))
        (funcs (cdr forbidden))
        (to (ensure-directories-exist
	     (concatenate 'string (directory-namestring from) *parent-folder* folder-name qlabel ".lisp"))))
    (with-open-file (out to :direction :output :if-exists :supersede)
      (emit-code out `(forbidden-symbols :penalty ,penalty :symbols (quote ,funcs)))
      (format out "~%")
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
  (let ((ht (make-hash-table)))
    (comp-exam exam-specs ht)
    (maphash (lambda (k v)
               (let ((qlabel (format nil "q~a" (question-number v)))
                     (forbidden (question-forbidden v))
                     (examples (question-examples v))
                     (test-cases (question-test-cases v)))
                 (gen-packages exam-specs qlabel test-cases)                   
                 (gen-tcs exam-specs qlabel forbidden examples "Examples/")
                 (gen-tcs exam-specs qlabel forbidden test-cases "Test-Cases/")
                 (format t "~%~a:~%Forbidden: ~a~%Examples: ~a~%TCs: ~a~%" qlabel forbidden examples test-cases))) ht)))
