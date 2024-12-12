
(defparameter *question-marker* "#+QUESTION:")

(defparameter *begin-examples-marker* "#+BEGIN_EXAMPLE")

(defparameter *end-examples-marker* "#+END_EXAMPLE")

(defparameter *begin-test-cases-maker* "#+BEGIN_TCS")

(defparameter *end-test-cases-maker* "#+END_TCS")

(defun sect-marker? (line str)
  (when (and (coerce line 'list) (>= (length line) (length str)))
    (let ((tkn (subseq (string-upcase line) 0 (length str))))
      (if (equal (string-upcase str) tkn) (subseq line (length str))))))

(defun get-qnumber (line)
  line)

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

(defun comp-exam (from)
  (if (probe-file from)
      (with-open-file (in from)
        (let ((question-flag nil)
              (examples-flag nil))
	  (loop for line = (read-line in nil nil)
		while line do
		  (format t ".")
		  (cond ((sect-marker? line *question-marker*)
                         (setf question-flag t)
                         (format t "~a~%" (get-item "forbid" (subseq line (length *question-marker*)))))))))))
