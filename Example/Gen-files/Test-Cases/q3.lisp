
(FORBIDDEN-SYMBOLS :PENALTY 0.8 :SYMBOLS '(COUNT MEMBER))

(DEFTEST TEST-REVERSE-LIST NIL
 (CHECK (EQUALP (REVERSE-LIST '(1 2 3 4 5)) '(5 4 3 2 1))
  (EQUALP (REVERSE-LIST 'NIL) 'NIL) (EQUALP (REVERSE-LIST '(A B C)) '(C B A))))

(DEFUN TEST-Q3 () (TEST-REVERSE-LIST) (FMAKUNBOUND 'REVERSE-LIST))

(TEST-Q3)