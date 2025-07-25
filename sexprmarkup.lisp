(in-package :sexprmark-to-org)

;; Global variables

(defparameter *parent-folder* "Gen-files/")

(defparameter *orgmode-markers* '("#+BEGIN_SRC lisp" "#+BEGIN_SRC shell" "#+END_SRC" "#+BEGIN_EXAMPLE" "#+END_EXAMPLE"))

(defvar *folder-name* nil)

;; Structure to store information about questions

(defstruct question
  number forbidden penalty description examples testcases given-functions)


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
  (when p
    (if (char= (aref p (1- (length p))) #\/)
        p
        (concatenate 'string p "/"))))

(defun char-whitespace-p (ch)
  "Returns T if CH is a common whitespace character."
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun only-blank-spaces-p (str)
  "Returns T if STR is empty or contains only whitespace characters, NIL otherwise."
  (every #'(lambda (ch) (char-whitespace-p ch)) str))

(defun cr-pairs (a)
  (if (null a) a
      (cons (list (second (first a)) (third (first a))) (cr-pairs (cdr a)))))

(defun str->list (str)
  (let ((stream (make-string-input-stream str)))
    (loop for line = (read-line  stream nil :eof)
          until (eq line :eof)
          collect line)))

(defun remove-substring (substring string)
  "Removes all occurrences of SUBSTRING from STRING."
  (with-output-to-string (out)
    (loop with len = (length substring)
          for start = 0 then (+ pos len)
          for pos = (search substring string :start2 start)
          while pos
          do (write-string string out :start start :end pos)
          finally (write-string string out :start start))))

(defun remove-substrings (lstr s)
  (let ((res s))
    (dolist (substring lstr res)
      (setf res (remove-substring substring res)))))

(defun normalize-whitespace (s)
  (with-output-to-string (out)
    (loop for ch across s
          with prev-space = nil
          do (cond
               ((char= ch #\Space)
                (unless prev-space (write-char #\Space out))
                (setf prev-space t))
               (t (write-char ch out)
                  (setf prev-space nil))))))

(defun keyword-symbol-p (obj)
  "Returns T if OBJ is a keyword symbol (a symbol starting with a colon),
   otherwise returns NIL."
  (and (symbolp obj)            ; Check if it's a symbol
       (eq (symbol-package obj) ; Check if it belongs to the KEYWORD package
           (find-package :keyword))))

(defun subst-package-symbols (form package-designator &optional symbs alt-package)
  "Adds the PACKAGE-DESIGNATOR to the name of all symbols in FORM that are not in the list SYMBS.
   If the symbol is in the list SYMBS then adds the ALT-PACKAGE designator to the name of the 
   symbol."
  (cond ((consp form)
         (mapcar #'(lambda (x) (subst-package-symbols x package-designator symbs alt-package)) form))
        ((keyword-symbol-p form) form)
        ((symbolp form)
         (if (member form symbs)
             (intern (symbol-name form) (find-package alt-package))
             (intern (symbol-name form) (find-package package-designator))))
        (t form)))



(defun pretty-print-to-string (form)
  "Pretty-prints a Lisp form to a string."
  (with-output-to-string (s)
    (let ((*standard-output* s) ; Bind *standard-output* to the string stream
          (*print-pretty* t))   ; Ensure pretty printing is enabled
      (pprint form))))

;; Serializer

(defun sexprmark->org (sexpr questions-info)
  (labels
      ((emit (node &key folder qnumber penalty function-name given-functions forbidden (depth 0) nitem mcq)
         "folder is the where students are required to store their solutions; qnumber is the question number; 
          penalty is the percentage deduction on a solution; forbidden is a list of forbidden functions;
          and depth is space indentation in items"
         (cond
           ((consp node)
            (case (car node)
              (doc
               (let* ((proplist (second node))
                      (title (getf proplist :title))
                      (folder (if title (check-foldername (getf proplist :folder))
                                  (error "Missing document title in ~s" node)))
                      (toc (if folder (getf proplist :toc)
                               (error "Missing folder location for student solutions in ~s" node)))
                      (num  (getf proplist :num))
                      (children (nthcdr 2 node)))
                 (setf *folder-name* folder)
                 (cons (format nil "#+TITLE: ~a~%" title)
                       (cons (format nil "#+Options: toc:~[nil~;t~] num:~[nil~;t~] date:nil author:nil~%" (if toc 1 0) (if num 1 0))
                             (mapcar (lambda (item) (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                                     children)))))             
              (s ;; Section
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
              (q ;; Question
               (let* ((proplist (cadr node))
                      (title (getf proplist :title))
                      (number (if title (getf proplist :number)
                                  (error "Missing question title in ~s" node)))
                      (penalty (if number (getf proplist :penalty)
                                   (error "Missing question number in ~s" node)))
                      (forbidden (getf proplist :forbidden))
                      (mcq (getf proplist :mcq))
                      (children (cond ((and forbidden (or (not penalty) (= penalty 0)))
                                       (error "You forgot to provide the penalty in ~s" node))
                                      ((and penalty (not (= penalty 0)) (not forbidden))
                                       (error "You forgot to provide the list of forbidden functions in ~s" node))
                                      (t (nthcdr 2 node)))))
                 (setf (gethash number questions-info) (make-question :number number :penalty penalty :forbidden forbidden))
                 (cons (emit '(p))
                       (cons (format nil "~a ~a ~a~%" (stars 1) title number)
                             (mapcar (lambda (item)
                                       (emit item :folder folder :qnumber number :penalty penalty :forbidden forbidden :depth depth :mcq mcq))
                                     children)))))
              (wa ;; Whats asked 
               (let ((description (append (list (emit `(s (:level 2 :title "WHAT YOU ARE ASKED")
                                                          (p (b "NOTE:"))
                                                          (ul
                                                           (li "You " are required to write the solutions for the parts of this question in the Lisp program file ,(format nil "*~aq~a.lisp* ." folder qnumber))
                                                           ,(if (not  mcq)                                                                
                                                                `(li "You " may create helper functions in your program file.)
                                                                "")
                                                           ,(if forbidden
                                                                `(li "You must not use or refer to the following Lisp built-in function(s) and symbol(s): " ,(format nil "~{*~a*~^, ~}" forbidden) ".  The penalty for doing so is a deduction of " (b ,penalty percent) " on the score of your solutions for this question")
                                                                "")
                                                           ,(if (not mcq)
                                                                `(li "To ensure your solution is in the correct folder and passes the test cases shown in the examples below,  type the following expression on the REPL:" (cb (:language "lisp") ,(format nil "(cg:chk-my-solution \"~aq~a.lisp\")" folder qnumber)))
                                                                "")))))
                                          (mapcar (lambda (item)
                                                    (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                                                  (cdr node)))))
                 (push (normalize-whitespace (remove-substrings *orgmode-markers*  (apply #'concatenate 'string  (flatten description)))) (question-description (gethash qnumber questions-info)))
                 description))
              (p ;; Paragraph
               (append (cons (format nil "~%~a" (indent depth))
                             (mapcar (lambda (item)
                                       (emit item :folder folder :qnumber qnumber :penalty penalty :forbidden forbidden :depth depth))
                                     (cdr node)))
                       (list (format nil "~%"))))
              (ul ;; Unnumbered items
               (mapcar (lambda (item)
                         (cons (format nil "~%") (emit item :depth (1+ depth) :qnumber qnumber)))
                       (cdr node)))
              (ol ;; numbered items
               (let* ((proplist (second node))
                      (startp (equalp (first proplist) :start))
                      (start (if startp
                                 (getf proplist :start)
                                 1))
                      res)
                 (dolist (item (if startp (cddr node) (cdr node)) (reverse res))
                   (push (cons (format nil "~%") (emit item :depth (1+ depth) :nitem start :qnumber qnumber))
                         res)
                   (incf start))))
              (li ;; item
               (let ((item (cdr node)))
                 (cond
                   ((and (consp (car item)) (eq (caar item) 'todo))
                    (list (format nil "~a- [ ] ~{~a~}" (indent depth) (flatten (mapcar (lambda (e)
                                                                                         (emit e :depth depth :qnumber qnumber))
                                                                                       (cdar item))))))
                   ((and nitem (consp item))
                    (list (format nil "~a~a. [@~a] ~{~a~}" (indent depth) nitem  nitem (flatten (mapcar (lambda (e)
                                                                                                          (emit e :depth depth :nitem (1+ nitem) :qnumber qnumber))
                                                                                                        item)))))
                   ((consp item)
                    (list (format nil "~a- ~{~a~}" (indent depth) (flatten (mapcar (lambda (e)
                                                                                     (emit e :depth depth :qnumber qnumber))
                                                                                   item)))))
                   (t (error "Improper item ~a" item)))))
              (hl ;; Hyper link
               (let ((link (second node))
                     (text (third node)))
                 (list (format nil (if text  "[[~a][~a]] " "[[~a]]") link text))))
              (b ;; Bold font
               (list (format nil "*~{~a~}* " (trim-spc-last (flatten (mapcar #'emit (cdr node)))))))
              (tt ;; True type font
               (list (format nil "=~{~a~}= " (trim-spc-last (flatten (mapcar #'emit (cdr node)))))))
              (em ;; Italics
               (list (format nil "/~{~a~}/ " (trim-spc-last (flatten (mapcar #'emit  (cdr node)))))))
              (rp ;; Parenthesized 
               (list (format nil "(~{~a~}) " (trim-spc-last (flatten (mapcar #'emit  (cdr node)))))))
              (tc
               (let* ((proplist (second node))
                      (function-name (getf proplist :function))
                      (gf (getf proplist :given))
                      (given-functions (if (listp gf) gf (list gf))))
                 (unless function-name
                   (error "Missing function name key in test cases ~s" node))
                 (unless qnumber
                   (error "Test cases not inside a question ~s" node))
                 (when given-functions
                   (let ((new-list (union given-functions (question-given-functions (gethash qnumber questions-info)))))
                     (setf (question-given-functions (gethash qnumber questions-info)) new-list)))
                 (mapcar (lambda (item)
                           (emit item :depth depth :function-name function-name :qnumber qnumber :given-functions (if (listp given-functions) given-functions (list given-functions))))
                         (nthcdr 2 node))))
              ((gvn hdn) ;; Given test cases , Hidden test cases
               (if (equalp (car node) 'gvn)
                   (push (append (list 'deftest function-name) (cdr node))
                         (question-examples (gethash qnumber questions-info)))
                   (push (append (list 'deftest function-name) (cdr node))
                         (question-testcases (gethash qnumber questions-info))))
               (when (equalp (car node) 'gvn)
                 (cons (format nil "~%~a#+BEGIN_SRC lisp" (indent depth))
                       (append (mapcar (lambda (item) (emit item :depth (1+ depth) :function-name function-name :qnumber qnumber)) (cdr node))
                               (list (format nil "~%~a#+END_SRC~%" (indent depth)))))))
              (a ;; Assertion in a testcase or example block
               (unless (= (length (cdr node)) 2)
                 (error "Incorrect use of Assertion: ~a" node))
               (unless function-name
                 (error "Assertion not inside a test case ~s" node))  
               (let* ((expected (second node))
                      (result (eval (third node))))
                 (list (format nil "~%The expression below~% ~a~%~%should evaluate to~%~a~%" (pretty-print-to-string expected)  (pretty-print-to-string result)))))
              (cb ;; Code block
               (let* ((proplist (second node))
                      (lang (getf proplist :language))
                      (code (if lang (third node)
                                (error ":LANGUAGE attribute missing from Code Block: ~a" node))))
                 (cons (format nil "~%~a#+BEGIN_SRC ~a" (indent depth) lang)
                       (append (mapcar (lambda (line)
                                         (format nil "~%~a~a" (indent (* 1 depth)) line)) (str->list code))
                               (list (format nil "~%~a#+END_SRC~%" (indent (* 1 depth))))))))
              (t (format nil "Invalid node: ~s" node))))
           ((symbolp node) (list (format nil "~a " (string-downcase (symbol-name node)))))
           ((stringp node) (list (format nil "~{~a~^~%~}" (str->list node))))
           ((atom node) (list (format nil "~a " node))))))
    (format nil "~{~a~}" (flatten (emit sexpr)))))

(defun gen-tc-code (qlabel cases)
  (let (fm-names
        deftests)
    (append
     (dolist (g-cases  cases (cons `(declaim (notinline ,@(mapcar #'car fm-names))) deftests))
       (let ((fm-name-cases (list (second g-cases) (intern (format nil "TEST-~a" (second g-cases))) (cr-pairs (cddr g-cases)))))
         (push fm-name-cases fm-names)
         (push `(deftest ,(second fm-name-cases) ()
                  (check
                    ,@(let ((res)
                            (cases (cddr g-cases)))
                        (dolist (e cases (reverse res))
                          (push `(equalp ,(cadr e) ,(caddr e)) res)))))
               deftests))) 
     (list `(defun ,(intern (format nil "TEST-~a" (string-upcase qlabel))) () 
              ,@(let ((res)
                      (rfm-names (reverse fm-names)))
                  (dolist (e rfm-names)
                    (push (list (second e)) res))
                  (dolist (e rfm-names (reverse res))
                    (push (list 'fmakunbound  (list 'quote (first e))) res))))))))

(defun gen-tcs (qnumber description forbidden penalty examples testcases hidden)
  (let ((qlabel (format nil "q~a" qnumber)))
    `(,qlabel ("whats-asked" (,@description))
              ,(if forbidden
                   `("forbidden-symbols" :penalty ,penalty :symbols (,@forbidden)))
              ("given" ,@(gen-tc-code qlabel examples))
              ,(if hidden
                   `("hidden" ,@(gen-tc-code qlabel testcases))))))

(defun gen-exam-files (from &key hidden)
  "From is the file containing the assessment's sexprmarkup description"
  (let* ((fn-ext (pathname-type from))
         (assessment-sexpr (if (and fn-ext (string= fn-ext "sxm"))
                               (with-open-file (in from)
                                 (read in))
                               (error "File name does not have the extension '.sxm': ~a" from)))
         (questions-info (make-hash-table))
         (questions)
         (orgmode-version (ensure-directories-exist
		           (concatenate 'string (directory-namestring from) *parent-folder* (format nil "~a.org" (pathname-name (file-namestring from))))))
         (exam-data (ensure-directories-exist
		    (concatenate 'string (directory-namestring from) *parent-folder* (format nil "~a.data" (pathname-name (file-namestring from))))))
         all-fnames
         tcs-driver)
    (with-open-file (out orgmode-version :direction :output :if-exists :supersede)
      (format out "~a" (sexprmark->org assessment-sexpr questions-info)))
    (format t "~%Generated assessment orgmode description file at: ~a" orgmode-version)
    (maphash (lambda (k v)
               (push  (gen-tcs k (question-description v ) (question-forbidden v) (question-penalty v) (question-examples v) (question-testcases v) hidden)
                      tcs-driver)
               (push (format nil "q~a" k) questions)
               (setf all-fnames (append (mapcar #'second (question-examples v)) (question-given-functions v) all-fnames)))
             questions-info)
    (with-open-file (out exam-data :direction :output :if-exists :supersede)
      (format out "~s" (cons (list "folder" *folder-name*)
                             (cons (list "questions" (reverse questions))
                                   (cons (list "fnames" (reverse all-fnames)) (reverse tcs-driver))))))
    (format t "~%Generated assessment testing code at: ~a~%Done." exam-data)))

