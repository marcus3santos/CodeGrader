
(FORBIDDEN-SYMBOLS :PENALTY NIL :SYMBOLS 'NIL)

(DEFTEST TEST-REVERSE-LIST NIL
 (CHECK (EQUALP (REVERSE-LIST '(1 2 3 4 5)) '(5 4 3 2 1))
  (EQUALP (REVERSE-LIST 'NIL) 'NIL) (EQUALP (REVERSE-LIST '(A B C)) '(C B A))))

(DEFTEST TEST-PALINDROME NIL
 (CHECK (EQUALP (PALINDROME? '(X Y X)) T) (EQUALP (PALINDROME? '(X Y Z)) NIL)
  (EQUALP (PALINDROME? 'NIL) T)))

(DEFUN TEST-Q2 ()
  (TEST-REVERSE-LIST)
  (TEST-PALINDROME)
  (FMAKUNBOUND 'REVERSE-LIST)
  (FMAKUNBOUND 'PALINDROME))

(TEST-Q2)