(defun count-occurrences (n list)
  (cond ((null list) 0)
        ((= (car list) n) (+ 1 (count-occurrences n (cdr list))))
        (t (count-occurrences n (cdr list)))))
