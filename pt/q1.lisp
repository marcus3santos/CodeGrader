;; After the runtime error is triggered, Codegrader leaves the  user in the
;; :test-runtime package. It should return the user to the original package
;; where chk-... was launched.

(defun split-even-odd (a)
  nil
  (split-even-odd b))

;;(delete 1 '(1 2))

