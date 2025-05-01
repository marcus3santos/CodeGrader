(defun reverse-list (a &optional res)
  (if (null a) res
      (reverse-list (cdr a) (cons (car a) res))))


(defun palindrome? (a)
  (equalp a (reverse a)))
