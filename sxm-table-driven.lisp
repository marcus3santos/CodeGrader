(defstruct compiler-state
  tag-compilers     ;; hash-table
  metadata          ;; hash-table
  tags              ;; The list of tags
  env               ;; environment 
  )


(defun make-tag-table ()
  (let ((table (make-hash-table)))
    (register-core-tags table)
    table))

(defun make-initial-state ()
  (make-compiler-state
   :tag-compilers (make-tag-table)
   :metadata (make-hash-table :test #'equal)
   :tags '(:doc :q :s :p :ul :ol :li :wa :tc :gvn :hdn :a :cb :sols :sol
           doc q s p ul ol li wa tc gvn hdn a cb sols sol)
   :env (list :level 0  :ol-p nil :i-num 0)))

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

(defun register-core-tags (table)

  (register-tag table 'doc-tag
                (deftag doc-tag (args state)
                  (destructuring-bind (props &rest body) args
                    (multiple-value-bind (body-text st)
                        (compile-nodes body state)
                      (let* ((title (getf props :title ))
                             (folder (check-folder-name (getf props :folder))))
                        (setf (gethash "folder" (compiler-state-metadata st)) folder)
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
                      (values (format nil "~a~%" body-text) st)))))

  (register-tag table 'ol-tag
                (deftag ol-tag (args state) 
                  (destructuring-bind (&rest body) args
                    (let* ((ol-flag (getf (compiler-state-env state) :ol-p))
                           (cur-i-num (getf (compiler-state-env state) :i-num))
                           (cur-level (getf (compiler-state-env state) :level)))
                      (incf (getf (compiler-state-env state) :level))
                      (incf (getf (compiler-state-env state) :i-num))
                      (setf (getf (compiler-state-env state) :ol-p) t)
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (setf (getf (compiler-state-env state) :i-num) cur-i-num)
                        (setf (getf (compiler-state-env state) :level) cur-level)
                        (setf (getf (compiler-state-env state) :ol-p) ol-flag)
                        (values body-text st))))))

  (register-tag table 'li-tag
                (deftag li-tag (args state)
                  (destructuring-bind (&rest body) args
                    (let* ((level (getf (compiler-state-env state) :level))
                           (indent (* level 2))
                           (i-num  (getf (compiler-state-env state) :i-num))
                           (bullet (or (and :ol-p (format nil "~a. " inum))
                                       "- ")))
                      (multiple-value-bind (body-text st)
                          (compile-nodes body state)
                        (values
                         (format t "~a~a~a" (make-string indent :initial-element #\ ) bullet body-text)
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
                    (multiple-value-bind (body-text st)
                        (compile-nodes body state)
                      (let* ((number (getf props :number))
                             (forbidden (getf props :forbidden))
                             (penalty (getf props :penalty))
                             (q-label (format nil "q~a" number))
                             (q-labels-list (gethash "questions" (compiler-state-metadata st)))
                             (q-data (gethash q-label (compiler-state-metadata st)))
                             (q-new (cons q-label q-labels-list))
                             (new-q-data (when (and penalty forbidden)
                                           (push (list "forbidden-symbols"
                                                       :penalty penalty
                                                       :symbols forbidden)
                                                 q-data))))
                        (setf (gethash "questions" (compiler-state-metadata st)) q-new
                              (gethash q-label (compiler-state-metadata st)) new-q-data)
                        (values
                         (format nil "~%* ~a ~d~%~a" 
                                 (getf props :title)
                                 number
                                 body-text)
                         st))))))


  (register-tag table 'wa-tag
                (deftag wa-tag (args state)
                  (destructuring-bind (&rest body) args
                    (let* ((q-labels-list-symb (gethash "questions" (compiler-state-metadata state)))
                           (q-label-symb (first q-labels-list-symb))
                           (q-data-symb (gethash q-label-symb (compiler-state-metadata state)))
                           (folder-name (gethash "folder" (compiler-state-metadata state))))
                      (multiple-value-bind (body-text st)
                          (compile-nodes
                           `(s-tag (:level 2 :title "WHAT YOU ARE ASKED")
                                   (p-tag "*NOTE*:")
                                   (ul-tag
                                    (li-tag (format nil "You are required to write the solutions for the parts of this question in the lisp program file *~a~a.lisp* ."
                                                    ,folder
                                                    ,q-label-symb))
                                    (li-tag "You may create helper functions in your program file. ")
                                    (li-tag "To ensure your solution is in the correct folder and passes the test cases shown in the examples below,  type the following expression on the REPL:"
                                            (p-tag (cb-tag (:language "lisp")
                                                           (format nil "(cg:chk-my-solution \"~a~a.lisp\")"
                                                                   ,folder-name
                                                                   ,q-label-symb)))))
                                   ,@body)
                           state) 
                        (setf (gethash q-label-symb (compiler-state-metadata st))
                              (cons (list "whats-asked" body) q-data-symb))
                        (values
                         (format nil "~a" body-text)
                         st)))))))

#|
(defun rename-tags (markup)             ; ;
(if (consp markup)                      ; ;
(let* ((tag (car markup))               ; ;
(rest (cdr markup))                     ; ;
(tag-name (intern (format nil "~A-TAG" (symbol-name tag))))) ; ;
(cond ((and (member tag *sxm*)          ; ;
(or (eq 'a-tag tag-name)                ; ;
(eq 'sol-tag tag-name)))                ; ;
(cons tag-name rest))                   ; ;
((member tag *sxm*)                     ; ;
(cons tag-name (mapcar #'rename-tags rest))) ; ;
(t (cons tag (mapcar #'rename-tags rest))))) ; ;
markup))                                ; ;
|#

(defun compile-node (node state)
  (cond
    ((stringp node) (values node state))
    ((symbolp node) (values (symbol-name node) state))
    ((consp node)
     (let* ((tag (car node))
            (args (cdr node))
            (fn (gethash tag (compiler-state-tag-compilers state))))
       (unless fn
         (error "Unknown DSL tag: ~a" tag))
       (funcall fn args state)))
    (t (error "Invalid DSL form: ~a" node))))

(defun compile-nodes (nodes state)
  (let ((text "")
        (st state))
    (dolist (n nodes)
      (multiple-value-bind (ot new-st) (compile-node n st)
        (setf text (concatenate 'string text ot)
              st new-st)))
    (values text st)))


(defun compile-document (dsl-form)
  (let ((state (make-initial-state)))
    (multiple-value-bind (text final-state)
        (compile-node dsl-form state)
      (values
       text
       (compiler-state-metadata final-state)))))

