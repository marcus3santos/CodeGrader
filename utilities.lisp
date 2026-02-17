(in-package :utils)

(defun keyword-symbol-p (obj)
  "Returns T if OBJ is a keyword symbol (a symbol starting with a colon),
   otherwise returns NIL."
  (and (symbolp obj)            ; Check if it's a symbol
       (eq (symbol-package obj) ; Check if it belongs to the KEYWORD package
           (find-package :keyword))))

(defun subst-package-symbols (form package-designator &optional symbs alt-package)
  "Adds the PACKAGE-DESIGNATOR to the name of all symbols in FORM that are not in the list SYMBS.
   If the symbol is in the list SYMBS then adds the ALT-PACKAGE designator to the name of the 
   symbol."
  (cond ((consp form)
         (mapcar #'(lambda (x) (subst-package-symbols x package-designator symbs alt-package)) form))
        ((keyword-symbol-p form) form)
        ((symbolp form)
         (if (member form symbs)
             (intern (symbol-name form) (find-package alt-package))
             (intern (symbol-name form) (find-package package-designator))))
        (t form)))

(defun remove-substring (substring string)
  "Removes all occurrences of SUBSTRING from STRING."
  (with-output-to-string (out)
    (loop with len = (length substring)
          for start = 0 then (+ pos len)
          for pos = (search substring string :start2 start)
          while pos
          do (write-string string out :start start :end pos)
          finally (write-string string out :start start))))

(defun remove-substrings (lstr s)
  (let ((res s))
    (dolist (substring lstr res)
      (setf res (remove-substring substring res)))))

(defun normalize-whitespace (s)
  (with-output-to-string (out)
    (loop for ch across s
          with prev-space = nil
          do (cond
               ((char= ch #\Space)
                (unless prev-space (write-char #\Space out))
                (setf prev-space t))
               (t (write-char ch out)
                  (setf prev-space nil))))))

(defun str->list (str)
  (let ((stream (make-string-input-stream str)))
    (loop for line = (read-line  stream nil :eof)
          until (eq line :eof)
          collect line)))
