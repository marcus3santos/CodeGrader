
(WHATS-ASKED
 '("" "*WHAT YOU ARE ASKED*:" "" "*NOTE*:"
   "- You are required to write the solutions for the parts of this question in the Lisp program file *~/pt/q4.lisp*"
   "- You may create helper functions in your program file."
   "- You must not use or refer to the following Lisp built-in function(s) and symbol(s): COUNT, MEMBER. The penalty for doing so is a deduction of 80.0% on the score of your solutions for this question."
   "" "Write a function =palindrome?= that takes a list as an"
   "argument and returns =T= if the list is a palindrome (reads the same"
   "forwards and backwards), and =NIL= otherwise. You may not use"
   "=REVERSE= or =NREVERSE=." "" "" "" ""))

(FORBIDDEN-SYMBOLS :PENALTY 0.8 :SYMBOLS '(COUNT MEMBER))

(DEFTEST TEST-PALINDROME NIL
 (CHECK (EQUALP (PALINDROME? '(X Y X)) T) (EQUALP (PALINDROME? '(X Y Z)) NIL)
  (EQUALP (PALINDROME? 'NIL) T)))

(DEFUN TEST-Q4 () (TEST-PALINDROME) (FMAKUNBOUND 'PALINDROME))

(TEST-Q4)