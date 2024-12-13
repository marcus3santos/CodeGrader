;; comp-exam.lisp

(defparameter *parent-folder* "gen-files/")
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

(defun comp-exam (from ht)
  (if (probe-file from)
      (with-open-file (in from)
        (let ((to (ensure-directories-exist
		   (concatenate 'string (directory-namestring from) *parent-folder* (format nil "~a-description.org" (pathname-name (file-namestring from)))))))
	  (with-open-file (out to :direction :output
				  :if-exists :supersede)
            (let ((question-flag nil)
                  (examples-flag nil)
                  (an-example-flag nil)
                  (test-cases-flag nil)                  
                  (examples nil)
                  (an-example nil)
                  (test-cases nil))
	      (loop for line = (read-line in nil nil)
		    while line do
		      (format t ".")
		      (cond ((sect-marker? line *question-marker*)
                             (setf question-flag (get-qnumber (subseq line (length *question-marker*)))
                                   (gethash question-flag ht) (make-question :number question-flag))
                             (emit out (format nil "~a~a" *question-marker* question-flag)))
                            ((sect-marker? line *forbidden-marker*)
                             (setf (question-forbidden (gethash question-flag ht))
                                   (read-from-string (subseq line (length *forbidden-marker*)) nil nil)))
                            ((sect-marker? line *begin-examples-marker*) ;; Examples begin
                             (setf examples-flag t)
                             (emit out *begin-examples-marker*))
                            ((sect-marker? line *end-examples-marker*) ;; Examples end
                             (setf (question-examples (gethash question-flag ht))
                                   (reverse (push (reverse an-example) examples)))
                             (setf examples-flag nil
                                   examples nil
                                   an-example nil
                                   an-example-flag nil)
                             (emit out *end-examples-marker*))
                            ((and examples-flag
                                  (not an-example-flag)
                                  (sect-marker? line *REPL-marker*)) ;; Example begins
                             (setf an-example-flag t)
                             (emit out (format nil "~a~a" *REPL-marker* (subseq line (length *REPL-marker*))))
                             (push (read-from-string (subseq line (length *REPL-marker*)) nil nil)
                                   an-example))
                            ((and examples-flag
                                  an-example-flag
                                  (sect-marker? line *REPL-marker*)) ;; Example ends
                             (emit out (format nil "~a~a" *REPL-marker* (subseq line (length *REPL-marker*))))
                             (push (reverse an-example) examples)
                             (setf an-example (list (read-from-string (subseq line (length *REPL-marker*)) nil nil))))
                            ((and examples-flag an-example-flag)
                             (emit out line)
                             (let ((item (read-from-string line nil nil)))
                               (push item an-example)))
                            ((sect-marker? line *begin-test-cases-maker*)
                             (setf test-cases-flag t))
                            ((sect-marker? line *end-test-cases-maker*)
                             (setf test-cases-flag nil)
                             (setf (question-test-cases (gethash question-flag ht))
                                   (read-from-string (apply #'concatenate 'string (reverse test-cases)) nil nil))
                             (setf test-cases nil))
                            (test-cases-flag                             
                             (push line test-cases))
                            (t (emit out line))))))))))

(defun gen-exam-files (exam-specs)
  (let ((ht (make-hash-table)))
    (comp-exam exam-specs ht)
    (maphash (lambda (k v) (format t "~%Question: ~a Forbidden: ~a Examples: ~a Test cases: ~a~%"
                                             (question-number v)
                                             (question-forbidden v)
                                             (question-examples v)
                                             (question-test-cases v))) ht)))
