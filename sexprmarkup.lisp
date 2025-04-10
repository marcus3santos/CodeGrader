(defpackage :sexprmark-to-org
  (:use :cl)
  (:export :sexprmark->org))

(in-package :sexprmark-to-org)

;; Structure to store information about questions

(defstruct question
  number forbidden penalty description examples testcases)


;; Utility functions

(defun indent (n)
  (make-string (* 2 n) :initial-element #\Space))

(defun stars (n)
  (make-string n :initial-element #\*))

(defun flatten (lst)
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    ((append (flatten (car lst)) (flatten (cdr lst))))))

(defun trim-spc-last (strings)
  (let ((trimmed (append (butlast strings)
                         (list (string-right-trim " " (car (last strings)))))))
    trimmed))

(defun check-foldername (p)
  "Adds a / to the end of the folder name if it is not there already"
  (if (char= (aref p (1- (length p))) #\/)
      p
      (concatenate 'string p "/")))

;; Serializer

(defun sexprmark->org (sexpr)
  (let (res
        (questions-info (make-hash-table)))
    (labels
        ((emit (node &key folder qnumber penalty forbidden (depth 0))
           "folder is the where students are required to store their solutions; qnumber is the question number; 
          penalty is the percentage deduction on a solution; forbidden is a list of forbidden functions;
          and depth is space indentation in items"
           (cond
             ((consp node)
              (case (car node)
                (doc
                 (let* ((proplist (cadr node))
                        (title (getf proplist :title))
                        (folder (if title (check-foldername (getf proplist :folder))
                                    (error "Missing document title in ~s" node)))
                        (children (if folder (nthcdr 2 node)
                                      (error "Missing folder location for student solutions in ~s" node))))
                   (cons (format nil "#+TITLE: ~a~%" title)
                         (mapcar (lambda (item) (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                                 children))))             
                (section
                 (let* ((proplist (cadr node))
                        (level (getf proplist :level))
                        (title (if level (getf proplist :title)
                                   (error "Missing section level in ~s" node)))
                        (children (if title (nthcdr 2 node)
                                      (error "Missing section title in ~s" node))))
                   (cons (emit '(p))
                         (cons (format nil "~a ~a~%" (stars level) title)
                               (mapcar (lambda (item)
                                         (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                                       children)))))
                (question
                 (let* ((proplist (cadr node))
                        (title (getf proplist :title))
                        (number (if title (getf proplist :number)
                                    (error "Missing question title in ~s" node)))
                        (penalty (if number (getf proplist :penalty)
                                     (error "Missing question number in ~s" node)))
                        (forbidden (getf proplist :forbidden))
                        (children (cond ((and forbidden (or (not penalty) (= penalty 0)))
                                         (error "You forgot to provide the penalty in ~s" node))
                                        ((and penalty (not (= penalty 0)) (not forbidden))
                                         (error "You forgot to provide the list of forbidden functions in ~s" node))
                                        (t (nthcdr 2 node)))))
                   (setf (gethash number questions-info) (make-question :number number :penalty penalty :forbidden forbidden))
                   (cons (emit '(p))
                         (cons (format nil "~a ~a ~a~%" (stars 1) title number)
                               (mapcar (lambda (item)
                                         (emit item :folder folder :qnumber number :penalty penalty :forbidden forbidden :depth depth))
                                       children)))))
                (whats-asked
                 (append
                  (list (emit '(p (b "WHAT YOU ARE ASKED:")))
                        (emit '(p (b "NOTE:")))
                        (emit `(ul
                                (li "You" are required to write the solutions for the parts of this question in the Lisp program file ,(format nil "*~aq~a.lisp* ." folder qnumber))
                                (li "You" may create helper functions in your program file.)
                                ,(if forbidden
                                     `(li "You" must not use or refer to the following Lisp built-in "function(s)" and "symbol(s):" ,(format nil "~{*~a*~^, ~}" forbidden) ".  The" penalty for doing so is a deduction of (b ,penalty percent) on the score of your solutions for this question.)
                                     `(li "There" are no restrictions in the use of Lisp built-in functions or symbols in the parts of this question.))
                                (li "To" ensure your solution is in the correct folder and passes the test cases shown in the examples "below," type the following expression on the "REPL:" (code-block :lang "lisp",(format nil "(chk-my-solution \"~aq~a.lisp\")" folder qnumber))))))
                  (mapcar (lambda (item)
                            (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                          (cdr node))))
                (p
                 (append (cons (format nil "~%")
                               (mapcar (lambda (item) (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                                       (cdr node)))
                         (list (format nil "~%"))))
                (ul
                 (mapcar (lambda (item)
                           (cons (format nil "~%") (emit item :depth (1+ depth))))
                         (cdr node)))
                (li
                 (let ((item (cdr node)))
                   (cond
                     ((and (consp (car item)) (eq (caar item) 'todo))
                      (format nil "~a- [ ] ~{~a~}" (indent depth) (flatten (mapcar (lambda (e)
                                                                                     (emit e :depth depth))
                                                                                   (cdar item)))))
                     ((consp item)
                      (format nil "~a- ~{~a~}" (indent depth) (flatten (mapcar (lambda (e)
                                                                                 (emit e :depth depth))
                                                                               item))))
                     (t (error "Improper item ~a" item)))))
                (hl
                 (let ((link (second node))
                       (text (third node)))
                   (list (format nil (if text  "[[~a][~a]] " "[[~a]]") link text))))
                (b
                 (list (format nil "*~{~a~}* " (trim-spc-last (mapcar #'emit (cdr node))))))
                (tt
                 (list (format nil "=~{~a~}= " (trim-spc-last (mapcar #'emit (cdr node))))))
                (em
                 (list (format nil "/~{~a~}/ " (trim-spc-last (mapcar #'emit  (cdr node))))))
                (rp
                 (list (format nil "(~{~a~}) " (trim-spc-last (mapcar #'emit  (cdr node))))))
                ((example-block testcase-block)
                 (let ((function-name (getf (second node) :name)))
                   (unless function-name
                     (error "Missing function name key in node ~s" node))
                   (unless qnumber
                     (error "Example block not inside a question ~s" node))
                   (if (equalp (car node) 'example-block)
                       (push (append (list 'deftest function-name) (cddr node))
                             (question-examples (gethash qnumber questions-info)))
                       (push (append (list 'deftest function-name) (cddr node))
                             (question-testcases (gethash qnumber questions-info))))
                   (when (equalp (car node) 'example-block)
                     (cons (format nil "~a~%#+BEGIN_SRC lisp~%" (indent depth))
                           (append (mapcar (lambda (line) (format nil "~a~a~%" (indent depth) line))
                                           (mapcar (lambda (item) (emit item :depth depth)) (cddr node)))
                                   (list (format nil "~a#+END_SRC~%" (indent depth))))))))
                (assertion
                 (let ((expected (second node))
                       (result (third node)))
                   (format nil "CL-USER> ~a~%~a" expected result)))
                (code-block
                 (let ((lang (getf (cdr node) :lang))
                       (lines (cdddr node)))
                   (cons (format nil "~%~a#+BEGIN_SRC ~a~%" (indent (* 2 depth)) lang)
                         (append (mapcar (lambda (line) (format nil "~a~a~%" (indent (* 2 depth)) line)) lines)
                                 (list (format nil "~a#+END_SRC~%" (indent (* 2 depth))))))))
                (t (format nil "Invalid node: ~a" node))))
             ((symbolp node) (format nil "~a " (string-downcase (symbol-name node))))
             ((atom node) (format nil "~a " node)))))
      (setf res (flatten (emit sexpr)))
      (maphash (lambda (k v)
                 (format t ">>> Question: ~a~%Forbidden: ~a~%Penalty: ~a~%Text: ~a~%Examples: ~a~%Testcases: ~a~%"
                         k (question-forbidden v) (question-penalty v) (question-description v) (question-examples v) (question-testcases v)))
               questions-info)
      (format nil "~{~a~}" res))))

(defun test ()
  (let ((sexpr
          '(doc (:title "PT 1" :folder "~/pt1/")
            (section  (:title "Heading 1" :level 1)
             (p "Some paragraph text under \"a\" heading 1.")
             (p "This" is a text.))
            (question (:title "Question" :number 1 :penalty 90 :forbidden ( a b c))
             (whats-asked
              (p "Question" comes here.)
              (example-block (:name fact)
               (assertion (fact 3) 6)
               (assertion (fact 0) 1)))
             (whats-asked
              (p "Question" comes here.)
              (example-block (:name square)
               (assertion (square 3) 9)
               (assertion (square 0) 0))))
            (question (:title "Question" :penalty 10 :number 2 :forbidden (a))
             (whats-asked
              (p "Question" comes here.)
              (testcase-block (:name fact)
               (assertion (fact 4) 24)
               (assertion (fact 0) 1))))
            (section (:level 2 :title "Heading 2")
             (p "Another" paragraph  "$x=2$"  and (em the rest.))
             (P "Write" a (tt function)  (b count-occurrences and another) )
             (ul
              (li "Item" one)
              (li "Item" two
               (ul
                (li "Subitem"
                 (ul
                  (li "Subsubitem")))
                (li "Another" item)))
              (li (todo "Task")))
             (p)
             (code-block :lang "python"
              "print(\"Hello, Org!\")")))))
    (with-open-file (out "~/tmp/q1.org" :direction :output :if-exists :supersede)
      (format out "~a" (sexprmark->org sexpr)))))
