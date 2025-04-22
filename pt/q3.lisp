(defun reverse-list (a &optional res)
  (if (null a) (reverse-list a)
      (reverse-list (cdr a) (cons (car a) res))))
