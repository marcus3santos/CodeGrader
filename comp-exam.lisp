
(defparameter *question-marker* "#+QUESTION:")

(defparameter *begin-examples-marker* "#+BEGIN_EXAMPLE")

(defparameter *end-examples-marker* "#+END_EXAMPLE")

(defparameter *begin-test-cases-maker* "#+BEGIN_TCS")

(defparameter *end-test-cases-maker* "#+END_TCS")

(defun sect-marker? (line str)
  (when (and (coerce line 'list) (>= (length line) (length str)))
    (let ((tkn (subseq (string-upcase line) 0 (length str))))
      (if (equal (string-upcase str) tkn) (subseq line (length str))))))

(defun get-qnumber-forbid (line))
(defun comp-exam (from)
  (if (probe-file from)
      (with-open-file (in from)
	(let ((to (ensure-directories-exist
		   (concatenate 'string (directory-namestring from) "gen-files/" (file-namestring from)))))
	  (with-open-file (out to :direction :output
				  :if-exists :supersede)
	    (let ((question-flag nil)
                  (examples-flag nil))
	      (loop for line = (read-line in nil nil)
		    while line do
		      (format t ".")
		      (cond ((sect-marker? line "#+Question:")
                             (setf question-flag t)
                             (get-qnumber-forbid (subseq line (length ) ) line))))))))))
