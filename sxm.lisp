(defparameter *metadata* nil)

(defparameter *include-hidden-flag* t "Includes hidden test cases data in metadata")

(defparameter *in-hdn-flag* nil "Will be set to t when :hdn tag is compiled")

(defparameter *parent-folder* "Gen-files/")

(defparameter *sxm*
  '(:doc :q :s :p :ul :ol :li :wa :tc :gvn :hdn :a :cb :sols :sol
    doc q s p ul ol li wa tc gvn hdn a cb sols sol) )

(defun check-folder-name (p)
  "Adds a / to the end of the folder name if it is not there already"
  (when p
    (if (char= (aref p (1- (length p))) #\/)
        p
        (concatenate 'string p "/"))))


(defun transform-to-calls (markup)
  (if (consp markup)
      (let* ((tag (car markup))
             (rest (cdr markup))
             (macro-name (intern (format nil "~A-MACRO" (symbol-name tag)))))
        (cond ((and (member tag *sxm*)
                    (or (eq 'a-macro macro-name)
                        (eq 'sol-macro macro-name)))
               (cons macro-name rest))
              ((member tag *sxm*)
               (cons macro-name (mapcar #'transform-to-calls rest)))
              (t (cons tag (mapcar #'transform-to-calls rest)))))
      markup))

(defun compile-markup (markup)
  "Main entry point to reset state and evaluate the markup."
  (setf *metadata* (make-hash-table :test #'equal))
  (let ((result (eval (transform-to-calls markup)))
        acc)
    (maphash (lambda (k v)
                               (push (list k v) acc))
                             *metadata*)
    (list :org-text result
          :metadata acc)))

;; --- Macro Definitions ---

;; Utility

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro doc-macro (props &rest body)
  `(doc-macro* *metadata* ,props ,@body))

(defmacro doc-macro* (metadata props &rest body) 
  `(progn
     (setf (gethash "folder" ,metadata) (check-folder-name (getf ',props :folder)))
     (format nil "#+TITLE: ~A~%#+OPTIONS: toc:nil num:nil date:nil author:nil~%~{~a~^~%~}" 
             (getf ',props :title) 
             (list ,@body))))

(defmacro s-macro (props &body body)
  "Handles Sections. Level 1 is *, Level 2 is **, etc."
  `(format nil "~%~a ~a~%~%~{~a~^~%~}"
           (make-string (or (getf ',props :level) 1) :initial-element #\*)
           (getf ',props :title "A title")
           (list ,@body)))


(defmacro q-macro (props &rest body)
  `(q-macro* *metadata* ,props ,@body))

(defmacro q-macro* (metadata props &rest body)
  (with-gensyms (q-label
                 q-labels-list
                 q-data
                 q-new
                 new-q-data
                 forbidden
                 penalty
                 number)
    `(let* ((,number (getf ',props :number))
            (,forbidden (getf ',props :forbidden))
            (,penalty (getf ',props :penalty))
            (,q-label (format nil "q~a" ,number))
            (,q-labels-list (gethash "questions" ,metadata))
            (,q-data (gethash ,q-label ,metadata))
            (,q-new (cons ,q-label ,q-labels-list))
            (,new-q-data (when (and ,penalty ,forbidden)
                          (push (list "forbidden-symbols"
                                      :penalty ,penalty
                                      :symbols ,forbidden)
                                ,q-data))))
       (setf (gethash "questions" ,metadata) ,q-new)
       (setf (gethash ,q-label ,metadata) ,new-q-data)
       (format nil "~%* ~a ~d~%~{~a~^~%~}" 
               (getf ',props :title)
               (getf ',props :number) 
               (list ,@body)))))

(defmacro wa-macro (&rest body)
  `(wa-macro* *metadata* ,@body))

(defmacro wa-macro* (metadata &body body)
  "The What's Asked section. Produces an orgmode subsection whose preamble informs the students 
   where they should save their program file and how they could test it."
  (with-gensyms (q-labels-list-symb
                 q-label-symb 
                 q-data-symb)
    `(let* ((,q-labels-list-symb (gethash "questions" ,metadata))
            (,q-label-symb (first ,q-labels-list-symb))
            (,q-data-symb (gethash ,q-label-symb ,metadata)))
       (setf (gethash ,q-label-symb ,metadata)
             (cons (list "whats-asked" ,@body) ,q-data-symb))
       (s-macro (:level 2 :title "WHAT YOU ARE ASKED")
           (p-macro "*NOTE*:")
           (ul-macro
            (li-macro (format nil "You are required to write the solutions for the parts of this question in the lisp program file *~a~a.lisp* ."
                         (gethash "folder" ,metadata)
                         ,q-label-symb))
            (li-macro "You may create helper functions in your program file. ")
            (li-macro "To ensure your solution is in the correct folder and passes the test cases shown in the examples below,  type the following expression on the REPL:"
              (p-macro (cb-macro (:language "lisp")
                           (format nil "(cg:chk-my-solution \"~a~a.lisp\")"
                                   (gethash "folder" ,metadata)
                                   ,q-label-symb)))))
         ,@body))))

(defmacro tc-macro (props &rest body)
  `(tc-macro* *metadata* ,props ,@body))

(defmacro tc-macro* (metadata props &body body)
  "Parent: Sets the lexical context for the function name."
  (with-gensyms (latest-q q-data fname fnames-data)
    `(let* ((,latest-q (first (gethash "questions" ,metadata)))
            (,q-data (gethash ,latest-q ,metadata))
            (,fname (getf ',props :function))
            (,fnames-data (gethash "fnames" ,metadata)))
       (setf (gethash ,latest-q ,metadata)
             (cons (list "asked-functions" (list ,fname))
                   ,q-data))
       (unless (member ,fname ,fnames-data)
         (setf (gethash "fnames" ,metadata) (cons ,fname ,fnames-data)))
       (format nil "~%~{~a~^~%~}"
               (list ,@body)))))


(defmacro gvn-macro (&rest body)
  `(gvn-macro* *metadata* ,@body))

(defun gen-tc-macros (q-label fname body)
  (let ((suite-name (intern (format nil "TEST-~:@(~a~)" q-label)))
        (fname-symb (if (stringp fname)
                        (intern (format nil "~:@(~A~)" fname))
                        fname))
        (test-name (intern (format nil "TEST-~:@(~A~)" fname))))
    `((declaim (notinline ,fname))
      (deftest ,test-name ()
        (check
          ,@(mapcar (lambda (a)
                      (list 'equalp (second a) (third a)))
                    body)))
      (defun ,suite-name ()
        (,test-name)
        (fmakunbound (quote ,fname-symb))))))

(defmacro gvn-macro* (metadata &rest body)
  "Given tests"
  (with-gensyms (latest-q-data        
                 q-label
                 fun-name
                 gvn-data
                 macros)
    `(let* ((,q-label (first (gethash "questions" ,metadata)))
            (,latest-q-data (gethash ,q-label ,metadata))
            (,fun-name (first (second (assoc "asked-functions" ,latest-q-data :test #'string=))))
            (,macros (gen-tc-macros ,q-label ,fun-name ',body))
            (,gvn-data (append (list "given") ,macros)))
       (setf (gethash ,q-label ,metadata)
             (cons ,gvn-data ,latest-q-data))
       (format nil "#+BEGIN_SRC lisp~%~{~a~^~%~}~%#+END_SRC"
               (list ,@body)))))

(defmacro hdn-macro (&rest body)
  `(hdn-macro* *in-hdn-flag* *include-hidden-flag* *metadata* ,@body))

(defmacro hdn-macro* (in-hdn-flag include-hidden-flag metadata &rest body)
  "Hidden tests"
  (with-gensyms (q-label
                 latest-q-data
                 fun-name
                 macros
                 hdn-data)
    `(progn
       (setf ,in-hdn-flag t)
       (when ,include-hidden-flag
             (let* ((,q-label (first (gethash "questions" ,metadata)))
                    (,latest-q-data (gethash ,q-label ,metadata))
                    (,fun-name (first (second (assoc "asked-functions" ,latest-q-data :test #'string=))))
                    (,macros (gen-tc-macros ,q-label ,fun-name ',body))
                    (,hdn-data (append (list "hidden") ,macros)))
               (setf (gethash ,q-label ,metadata)
                     (cons ,hdn-data ,latest-q-data))
               (list ,@body)))
       (setf ,in-hdn-flag nil)
       "")))

(defmacro a-macro (call expected)
  `(a-macro* *in-hdn-flag*  ',call  ',expected))

(defmacro a-macro* (in-hdn-flag call expected)
  "The leaf: uses '%internal-sxm-context-fn-name%' shared by 'tc'."
  `(if ,in-hdn-flag
       ""
       (format nil "- The expression below~%~%    ~s~%~%  should evaluate to~%~%    ~s~%" 
               ,call ,expected)))

(defmacro p-macro (&rest content)
  "Paragraph macro that handles strings or nested tags (UL, CB, etc.)."
  `(format nil "~{~a~^~%~}~%" 
           (list ,@(mapcar (lambda (item)
                             (if (stringp item)
                                 item
                                 item))
                           content))))

(defmacro ol-macro (&body items)
  "Ordered lists. For now only unordered lists"
  `(ul-macro ,@items))

(defmacro ul-macro (&body items)
  "Unordered lists."
  `(format nil "~{~a~^~%~}" (list ,@items)))

(defmacro li-macro (text &body nested)
  "List items. Supports nested content."
  `(format nil "- ~a~{~%  ~a~}" ,text (list ,@nested)))

(defmacro cb-macro (props text)
  "Code blocks (Source blocks in Org)."
  `(format nil "~%#+BEGIN_SRC ~a~%~a~%#+END_SRC~%" 
           (getf ',props :language) 
           ,text))

(defmacro sols-macro (&rest body)
  `(sols-macro* *include-hidden-flag* *metadata* ,@body))

(defmacro sols-macro* (include-hidden-flag metadata &rest body)
  "Container for solutions."
  (with-gensyms (q-label
                 latest-q-data
                 rmv-sols-q-data
                 sols-data)
    `(progn
       (when ,include-hidden-flag
         (let* ((,q-label (first (gethash "questions" ,metadata)))
                (,latest-q-data (gethash ,q-label ,metadata))
                (,rmv-sols-q-data (remove "solutions" ,latest-q-data :key #'first :test #'string=))
                (,sols-data (list "solutions" ,@body)))
           (setf (gethash ,q-label ,metadata)
                 (cons ,sols-data ,rmv-sols-q-data))
           (list ,@body)))
       "")))

(defmacro sol-macro (&rest body)
  `(sol-macro* *include-hidden-flag* ,@body))

(defmacro sol-macro* (include-hidden-flag &body body)
  "An individual solution block. Wraps code in Emacs Lisp/Common Lisp src blocks."
  `(when ,include-hidden-flag
     '(sol ,@body)))

(defparameter *input-data*
  '(:doc (:title "Exame" :folder "~/tmp")
         (:q (:number 1 :title "Question")
             (:wa
              (:p "Implement a SUM function.")
              (:tc (:function sum)
                   (:gvn
                    (:a (sum 0 1) 1)
                    (:a (sum 0 0) 0) )))
             (:sols
              (:sol (defun sum (a b) (+ a b))))
             )))
         


(let ((output (compile-markup *input-data*)))
  (format t "--- ORG OUTPUT ---~%~A" (getf output :org-text))
  (format t "--- METADATA ---~%~S" (getf output :metadata)))


(defun gen-exam-files (from &key include-hidden)
  "From is the .sxm file containing the assessment's description"
  (setf *include-hidden-flag* include-hidden)
  (let* ((fn-ext (pathname-type from))
         (assessment-sexpr (if (and fn-ext (string= fn-ext "sxm"))
                               (with-open-file (in from)
                                 (read in))
                               (error "File name does not have the extension '.sxm': ~a" from)))
         (orgmode-version (ensure-directories-exist
                           (format nil "~a~a~a.org"
                                   (directory-namestring from)
                                   *parent-folder*
                                   (pathname-name (file-namestring from)))))
         (exam-data (ensure-directories-exist
                     (format nil "~a~a~a.data"
                             (directory-namestring from)
                             *parent-folder*
                             (pathname-name (file-namestring from)))))
         (output (compile-markup assessment-sexpr))
         (orgmode-text (getf output :org-text))
         (metadata (getf output :metadata)))
    (with-open-file (out orgmode-version :direction :output :if-exists :supersede)
      (format out "~a" orgmode-text))
    (format t "~%Generated assessment orgmode description file at: ~a" orgmode-version)
    (with-open-file (out exam-data :direction :output :if-exists :supersede)
      (format out "~s" metadata))
    (format t "~%Generated assessment testing code at: ~a~%Done." exam-data)))
