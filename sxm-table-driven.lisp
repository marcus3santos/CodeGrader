(defstruct compiler-state
  tag-compilers     ;; hash-table
  metadata          ;; accumulator (an alist)
  tags              ;; The list of tags
  )


(defun make-tag-table ()
  (let ((table (make-hash-table)))
    (register-core-tags table)
    (maphash (lambda (k v)
               (format t "Key: ~a Value: ~a~%" k (describe v)))
             table)
    table))

(defun make-initial-state ()
  (make-compiler-state
   :tag-compilers (make-tag-table)
   :metadata (list)
   :tags '(:doc :q :s :p :ul :ol :li :wa :tc :gvn :hdn :a :cb :sols :sol
           doc q s p ul ol li wa tc gvn hdn a cb sols sol)))

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
  (register-tag
   table 'doc-tag
   (deftag doc-tag (args state)
     (destructuring-bind (props &rest body) args
       (multiple-value-bind (body-text st)
                        (compile-nodes body state)
         (let* ((title (getf props :title ))
                (folder (check-folder-name (getf props :folder))))
           (push `("folder" ,folder)
                 (compiler-state-metadata st))
           (values
            (format nil "#+TITLE: ~a~%#+OPTIONS: toc:nil num:nil date:nil author:nil~%~a"
                    title
                    body-text)
            st))))))
  
  (register-tag
   table 's-tag
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
              st)))))))


(defun rename-tags (markup)
  (if (consp markup)
      (let* ((tag (car markup))
             (rest (cdr markup))
             (tag-name (intern (format nil "~A-TAG" (symbol-name tag)))))
        (cond ((and (member tag *sxm*)
                    (or (eq 'a-tag tag-name)
                        (eq 'sol-tag tag-name)))
               (cons tag-name rest))
              ((member tag *sxm*)
               (cons tag-name (mapcar #'rename-tags rest)))
              (t (cons tag (mapcar #'rename-tags rest)))))
      markup))

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

