
(WHATS-ASKED
 '(" " "*WHAT YOU ARE ASKED*:" "*NOTE*:"
   "- You are required to write the solutions for the parts of this question in the Lisp program file *~/pt/q2.lisp*"
   "- You may create helper functions in your program file."
   "- You must not use or refer to the following Lisp built-in function(s) and symbol(s): COUNT, MEMBER. The penalty for doing so is a deduction of 80.0% on the score of your solutions for this question."
   "Write a function =contains-all?= that takes two lists as"
   "arguments and returns =T= if all elements of the first list are"
   "contained in the second list, and =NIL= otherwise."))

(FORBIDDEN-SYMBOLS :PENALTY 0.8 :SYMBOLS '(COUNT MEMBER))

(DEFTEST TEST-CONTAINS-ALL NIL
 (CHECK (EQUALP (CONTAINS-ALL? '(A B) '(A B C D E)) T)
  (EQUALP (CONTAINS-ALL? '(A Z) '(A B C D)) NIL)
  (EQUALP (CONTAINS-ALL? 'NIL 'NIL) T)))

(DEFUN TEST-Q2 () (TEST-CONTAINS-ALL) (FMAKUNBOUND 'CONTAINS-ALL))

(TEST-Q2)