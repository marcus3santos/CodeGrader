(defparameter *parent-folder* "Gen-files/")


(defstruct compiler-state
  tag-compilers   ;; hash-table
  metadata        ;; hash-table
  tags            ;; The list of tags
  tags-with-props ;; Tags that have a property list
  env             ;; environment 
  )


(defun make-tag-table ()
  (let ((table (make-hash-table)))
    (register-core-tags table)
    table))

(defun make-initial-state (include-hidden)
  (make-compiler-state
   :tag-compilers (make-tag-table)
   :metadata (make-hash-table :test #'equal)
   :tags '(:doc :q :s :p :ul :ol :li :wa :tc :gvn :hdn :a :cb :sols :sol
           doc q s p ul ol li wa tc gvn hdn a cb sols sol)
   :tags-with-props '(:doc doc :q q :s s :tc tc :cb cb)
   :env (list :q-num 0 :level -1  :ol-p nil :i-num 0 :in-hdn-p nil :include-hidden include-hidden)))

(defun register-tag (table name fn)
  (setf (gethash name table) fn))

(defmacro deftag (name (args state) &body body)
  (let ((fn-name (intern (format nil "COMPILE-TAG/~A" name))))
    `(progn
       (defun ,fn-name (,args ,state)
         ,@body)
       ',fn-name)))

(defun check-folder-name (p)
  "Adds a / to the end of the folder name if it is not there already"
  (when p
    (if (char= (aref p (1- (length p))) #\/)
        p
        (concatenate 'string p "/"))))

(defun gen-tc-code (q-label fname body)
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

(defun register-core-tags (table)
  "Registers the functions created by the deftag macro in the hash table
   using as the tags as key"
  (register-tag table 'doc-tag
                (deftag doc-tag (args state) 
                  (destructuring-bind (props &rest body) args
                    (let* ((title (getf props :title ))
                           (folder (check-folder-name (getf props :folder))))
                      (setf (gethash "folder" (compiler-state-metadata state)) folder)
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)                                                     
                        (values
                         (format nil "#+TITLE: ~a~%#+OPTIONS: toc:nil num:nil date:nil author:nil~%~a"
                                 title
                                 body-text)
                         st))))))
  
  (register-tag table 'p-tag
                (deftag p-tag (args state)
                  (destructuring-bind (&rest body) args
                    (multiple-value-bind (body-text st)
                        (compile-nodes body state)
                      (values (format nil "~%~a" body-text) st)))))

  (register-tag table 'ul-tag
                (deftag ul-tag (args state) 
                  (destructuring-bind (&rest body) args
                    (let* ((ol-flag (getf (compiler-state-env state) :ol-p))
                           (cur-i-num (getf (compiler-state-env state) :i-num))
                           (cur-level (getf (compiler-state-env state) :level)))
                      (incf (getf (compiler-state-env state) :level))
                      (setf (getf (compiler-state-env state) :i-num) 0)
                      (setf (getf (compiler-state-env state) :ol-p) nil)
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (setf (getf (compiler-state-env state) :i-num) cur-i-num)
                        (setf (getf (compiler-state-env state) :level) cur-level)
                        (setf (getf (compiler-state-env state) :ol-p) ol-flag)
                        (values (format nil "~a" body-text) st))))))

  (register-tag table 'ol-tag
                (deftag ol-tag (args state) 
                  (destructuring-bind (&rest body) args
                    (let* ((ol-flag (getf (compiler-state-env state) :ol-p))
                           (cur-i-num (getf (compiler-state-env state) :i-num))
                           (cur-level (getf (compiler-state-env state) :level)))
                      (incf (getf (compiler-state-env state) :level))
                      (setf (getf (compiler-state-env state) :i-num) 0)
                      (setf (getf (compiler-state-env state) :ol-p) t)
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (setf (getf (compiler-state-env state) :i-num) cur-i-num)
                        (setf (getf (compiler-state-env state) :level) cur-level)
                        (setf (getf (compiler-state-env state) :ol-p) ol-flag)
                        (values (format nil "~a" body-text) st))))))

  (register-tag table 'li-tag
                (deftag li-tag (args state)
                  (destructuring-bind (&rest body) args
                    (let* ((level (getf (compiler-state-env state) :level))
                           (indent (* level 2))
                           (i-num  (incf (getf (compiler-state-env state) :i-num)))
                           (o-flag (getf (compiler-state-env state) :ol-p))
                           (bullet (or (and  o-flag (format nil "~a. " i-num)) "- ")))
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (values
                         (format nil "~%~a~a~a" (make-string indent :initial-element #\ ) bullet body-text)
                         st))))))
  
  (register-tag table 's-tag
                (deftag s-tag (args state)
                  (destructuring-bind (props &rest body) args
                    (multiple-value-bind (body-text st)
                        (compile-nodes body state)
                      (let* ((level (getf props :level 1))
                             (title (getf props :title "A title")))
                        (values
                         (format nil "~%~a ~a~%~%~a"
                                 (make-string level :initial-element #\*)
                                 title
                                 body-text)
                         st))))))
  
  (register-tag table 'q-tag
                (deftag q-tag (args state)
                  (destructuring-bind (props &rest body) args
                    (let* ((number (incf (getf (compiler-state-env state) :q-num)))
                           (forbidden (getf props :forbidden))
                           (penalty (getf props :penalty))
                           (q-label (format nil "q~a" number))
                           (q-labels-list (gethash "questions" (compiler-state-metadata state)))
                           (q-data (gethash q-label (compiler-state-metadata state)))
                           (q-new (cons q-label q-labels-list))
                           (new-q-data (when (and penalty forbidden)
                                         (push (list "forbidden-symbols"
                                                     :penalty penalty
                                                     :symbols forbidden)
                                               q-data))))
                      (setf (gethash "questions" (compiler-state-metadata state)) q-new
                            (gethash q-label (compiler-state-metadata state)) new-q-data)
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state) 
                        (values
                         (format nil "~%* ~a ~d~%~a" 
                                 (getf props :title "Question")
                                 number
                                 body-text)
                         st))))))
  
  (register-tag table 'cb-tag
                (deftag cb-tag (args state)
                  (destructuring-bind (props &rest body) args
                    (multiple-value-bind (body-text st)
                        (compile-nodes body state)
                      (values
                       (format nil "~%#+BEGIN_SRC ~a~%~a~%#+END_SRC~%" 
                               (getf props :language) 
                               body-text)
                       st)))))
  
  (register-tag table 'wa-tag
                (deftag wa-tag (args state)
                  (destructuring-bind (&rest body) args
                    (let* ((metadata (compiler-state-metadata state))
                           (q-labels-list-symb (gethash "questions" metadata))
                           (q-label-symb (first q-labels-list-symb))
                           (folder-name (gethash "folder" metadata))
                           (str1 (format nil "You are required to write the solutions for the parts of this question in the lisp program file *~a~a.lisp* ."
                                         folder-name
                                         q-label-symb))
                           (str2 (format nil "(cg:chk-my-solution \"~a~a.lisp\")"
                                         folder-name
                                         q-label-symb)))
                      (multiple-value-bind (body-text st)
                          (compile-node
                           `(s-tag (:level 2 :title "WHAT YOU ARE ASKED")
                                   (p-tag "*NOTE*:")
                                   (ul-tag
                                    (li-tag ,str1)
                                    (li-tag "You may create helper functions in your program file. ")
                                    (li-tag "To ensure your solution is in the correct folder and passes the test cases shown in the examples below,  type the following expression on the REPL:"
                                            (p-tag (cb-tag (:language "lisp")
                                                           ,str2))))
                                   ,@body)
                           state) 
                        (setf (gethash q-label-symb (compiler-state-metadata st))
                              (cons (list "whats-asked" body-text)
                                    (gethash q-label-symb (compiler-state-metadata st))))
                        (values
                         (format nil "~a" body-text)
                         st))))))

  (register-tag table 'tc-tag
                (deftag tc-tag (args state)
                  (destructuring-bind (props &rest body) args
                    (let* ((metadata (compiler-state-metadata state))
                           (latest-q (first (gethash "questions" metadata)))
                           (q-data (gethash latest-q metadata))
                           (fname (getf props :function))
                           (fnames-data (gethash "fnames" metadata)))
                      (setf (gethash latest-q metadata)
                            (cons (list "asked-functions" (list fname))
                                  q-data))
                      (unless (member fname fnames-data)
                        (setf (gethash "fnames" metadata) (cons fname fnames-data)))
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (values (format nil "~%~a" body-text) st))))))

  (register-tag table 'gvn-tag
                (deftag gvn-tag (args state)
                  (destructuring-bind (&rest body) args
                    (let* ((metadata (compiler-state-metadata state))
                           (q-label (first (gethash "questions" metadata)))
                           (latest-q-data (gethash q-label metadata))
                           (fun-name (first (second (assoc "asked-functions" latest-q-data :test #'string=))))
                           (tc-code (gen-tc-code q-label fun-name body))
                           (gvn-data (append (list "given") tc-code)))
                      (setf (gethash q-label metadata)
                            (cons gvn-data latest-q-data))
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (values
                         (format nil "#+BEGIN_SRC lisp~&~a#+END_SRC"
                                 body-text)
                         st))))))
  
  (register-tag table 'hdn-tag
                (deftag hdn-tag (args state)
                  (destructuring-bind (&rest body) args
                    (setf (getf (compiler-state-env state) :in-hdn-p) t)
                    (when (getf (compiler-state-env state) :include-hidden)
                      (let* ((metadata (compiler-state-metadata state))
                             (q-label (first (gethash "questions" metadata)))
                             (latest-q-data (gethash q-label metadata))
                             (fun-name (first (second (assoc "asked-functions" latest-q-data :test #'string=))))
                             (tc-code (gen-tc-code q-label fun-name body))
                             (hdn-data (append (list "hidden") tc-code)))
                        (setf (gethash q-label metadata)
                              (cons hdn-data latest-q-data))))
                    (setf (getf (compiler-state-env state) :in-hdn-p) nil)
                    (values "" state))))
  
  (register-tag table 'a-tag
                (deftag a-tag (args state)
                  (destructuring-bind (call expected) args
                    (values
                     (if (getf (compiler-state-env state) :in-hdn-p)
                         ""
                         (format nil "- The expression below~%~%    ~s~%~%  should evaluate to~%~%    ~s~%~%" 
                                 call expected))
                     state))))

  (register-tag table 'sols-tag
                (deftag sols-tag (args state)
                  (destructuring-bind (&rest body) args
                    (when (getf  (compiler-state-env state) :include-hidden)
                      (let* ((metadata (compiler-state-metadata state))
                             (q-label (first (gethash "questions" metadata)))
                             (latest-q-data (gethash q-label metadata)))
                        (setf (gethash q-label metadata) (cons (list "solutions") latest-q-data))))
                    (multiple-value-bind (body-text st)
                        (compile-nodes body state)
                      (values body-text st)))))
  
  (register-tag table 'sol-tag
                (deftag sol-tag (args state)
                  (destructuring-bind (&rest body) args
                    (when (getf  (compiler-state-env state) :include-hidden)
                      (let* ((metadata (compiler-state-metadata state))
                             (q-label (first (gethash "questions" metadata)))
                             (latest-q-data (gethash q-label metadata))
                             (sols (rest (assoc "solutions" latest-q-data :test #'string=)))
                             (rmv-sols-q-data (remove "solutions" latest-q-data :key #'first :test #'string=))
                             (sols-data `("solutions" ,@(append sols (list `(sol ,@body))))))
                        (setf (gethash q-label metadata) (cons sols-data rmv-sols-q-data))))
                    (values ""  state)))))



(defun rename-tags (markup state)             
  (if (consp markup)                    
      (let* ((tag (car markup))         
             (rest (cdr markup))        
             (tag-name (intern (format nil "~A-TAG" (symbol-name tag))))) 
        (cond ((and (member tag (compiler-state-tags state))          
                    (or (eq 'a-tag tag-name)         
                        (eq 'sol-tag tag-name)))     
               (cons tag-name rest))
              ((and (member tag (compiler-state-tags state))
                    (member tag (compiler-state-tags-with-props state)))
               (cons tag-name (append (list (first rest))
                                      (mapcar (lambda (node)
                                                (rename-tags node state))
                                              (cdr rest)))))
              ((member tag (compiler-state-tags state))                    
               (cons tag-name (mapcar (lambda (node)
                                        (rename-tags node state))
                                      rest)))
              (t (cons tag (mapcar (lambda (node)
                                        (rename-tags node state))
                                      rest)))))
      markup))                               

(defun compile-node (node state)
  (cond
    ((stringp node) (values node state))
    ((symbolp node) (values (symbol-name node) state))
    ((consp node)
     (let* ((tag (first node))
            (args (rest node))
            (fn (gethash tag (compiler-state-tag-compilers state))))
       (unless fn
         (error "Unknown SXM tag: ~a" tag))
       (funcall fn args state)))
    (t (error "Invalid SXM form: ~a" node))))

(defun compile-nodes (nodes state)
  (let ((text "")
        (st state))
    (dolist (n nodes)
      (multiple-value-bind (ot new-st) (compile-node n st)
        (setf text (concatenate 'string text ot)
              st new-st)))
    (values text st)))


(defun compile-sxm-form (sxm-form include-hidden)
  (let* ((state (make-initial-state include-hidden))
         (new-sxm-form (rename-tags sxm-form state)))
    (multiple-value-bind (text final-state)
        (compile-node new-sxm-form state)
      (values text final-state))))

(defun org-to-html (input-file)
  "Converts an org-mode file to HTML using Emacs in batch mode."
  (let ((command (format nil "emacs --batch --visit=~A --funcall org-html-export-to-html --kill" 
                         (namestring (truename input-file)))))
    (uiop:run-program command :output t)))

(defun org-to-pdf (input-file)
  "Converts an org-mode file to PDF using Emacs and LaTeX."
  (let ((command (format nil "emacs --batch --visit=~A --funcall org-latex-export-to-pdf --kill" 
                         (namestring (truename input-file)))))
    (handler-case
        (uiop:run-program command :output t)
      (error (c)
        (format t "Error during PDF conversion: ~A~%" c)))))


(defun gen-exam-files (from &key include-hidden)
  "Generates the orgmode, html, pdf,  and metadata files from the .sxm file 
   containing the assessment's description. 
   If :INCLUDE-HIDDEN is T then the hidden test cases and question solutions
   will be added to the data file."
  (let* ((fn-ext (pathname-type from))
         (sxm-form (if (and fn-ext (string= fn-ext "sxm"))
                       (with-open-file (in from)
                         (read in))
                       (error "File name does not have the extension '.sxm': ~a" from)))
         (filename-root (format nil "~a~a~a"
                                (directory-namestring from)
                                *parent-folder*
                                (pathname-name (file-namestring from))))
         (orgmode-file (ensure-directories-exist (format nil "~a.org" filename-root)))
         (exam-data-file (ensure-directories-exist (format nil "~a.data" filename-root)))
         (html-file (format nil "~a.html" filename-root))
         (pdf-file (format nil "~a.pdf" filename-root)))
    (multiple-value-bind (orgmode-text state)
        (compile-sxm-form sxm-form include-hidden)
      (with-open-file (out orgmode-file :direction :output :if-exists :supersede)
        (format out "~a" orgmode-text))
      (format t "~&Assessment Org-mode file generated at: ~a" orgmode-file)
      (org-to-html orgmode-file)
      (format t "~&Assessment html file created at: ~a" html-file)
      (org-to-pdf orgmode-file)
      (format t "~&Assessment pdf file created at: ~a" pdf-file)
      (with-open-file (out exam-data-file :direction :output :if-exists :supersede)
        (format out "~s" (let (data)
                           (maphash (lambda (k v)
                                      (if (and (= (length k) 2)
                                               (char= (aref k 0) #\q))
                                          (push `(,k ,@v) data)
                                          (push (list k v) data)))
                                    (compiler-state-metadata state))
                           data)))
      (format t "~&Assessment metadata file created at: ~a~%" exam-data-file))))


