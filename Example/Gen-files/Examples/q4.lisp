
(FORBIDDEN-SYMBOLS :PENALTY 0.8 :SYMBOLS '(COUNT MEMBER))

(DEFTEST TEST-PALINDROME NIL
 (CHECK (EQUALP (PALINDROME? '(1 2 3 2 1)) T)
  (EQUALP (PALINDROME? '(A B C D)) NIL) (EQUALP (PALINDROME? 'NIL) T)))

(DEFUN TEST-Q4 () (TEST-PALINDROME) (FMAKUNBOUND 'PALINDROME))

(TEST-Q4)