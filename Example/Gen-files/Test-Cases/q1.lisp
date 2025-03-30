
(WHATS-ASKED
 '(" " "*WHAT YOU ARE ASKED*:" "*NOTE*:"
   "- You are required to write the solutions for the parts of this question in the Lisp program file *~/pt/q1.lisp*"
   "- You may create helper functions in your program file."
   "- You must not use or refer to the following Lisp built-in function(s) and symbol(s): COUNT, MEMBER. The penalty for doing so is a deduction of 80.0% on the score of your solutions for this question."
   "Write a function =count-occurrences= that takes an element"
   "and a list as arguments and returns the number of times the element"
   "appears in the list. For example:"))

(FORBIDDEN-SYMBOLS :PENALTY 0.8 :SYMBOLS '(COUNT MEMBER))

(DEFTEST TEST-COUNT-OCCURRENCES NIL
 (CHECK (EQUALP (COUNT-OCCURRENCES 1 '(1 1 1 1 1)) 5)
  (EQUALP (COUNT-OCCURRENCES 0 '(1 2 3 4)) 0)
  (EQUALP (COUNT-OCCURRENCES 'Z '(A B C Z Z)) 2)))

(DEFUN TEST-Q1 () (TEST-COUNT-OCCURRENCES) (FMAKUNBOUND 'COUNT-OCCURRENCES))

(TEST-Q1)