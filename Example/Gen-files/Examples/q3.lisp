
(WHATS-ASKED
 '("*WHAT YOU ARE ASKED*:" "*NOTE*:"
   "- You are required to write the solutions for the parts of this question in the Lisp program file *~/pt/q3.lisp*"
   "- You may create helper functions in your program file."
   "- You must not use or refer to the following Lisp built-in function(s) and symbol(s): COUNT, MEMBER. The penalty for doing so is a deduction of 80.0% on the score of your solutions for this question."
   "Write a function =reverse-list= that takes a list as an"
   "argument and returns a new list that is the reverse of the original"
   "list. "))

(FORBIDDEN-SYMBOLS :PENALTY 0.8 :SYMBOLS '(COUNT MEMBER))

(DEFTEST TEST-REVERSE-LIST NIL
 (CHECK (EQUALP (REVERSE-LIST '(1 2 3 4)) '(4 3 2 1))
  (EQUALP (REVERSE-LIST '(A B C)) '(C B A)) (EQUALP (REVERSE-LIST 'NIL) 'NIL)))

(DEFUN TEST-Q3 () (TEST-REVERSE-LIST) (FMAKUNBOUND 'REVERSE-LIST))

(TEST-Q3)