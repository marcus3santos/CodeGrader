(defparameter *max-depth* 10)

(defun replace-in-expr (s ns expr)
  (cond
    ((eq expr s) ns)
    ((listp expr)
     (cons (replace-in-expr s ns (car expr))  
           (mapcar (lambda (item) (replace-in-expr s ns item)) (cdr expr)))) 
    (t expr)))

(defun params2args (params &optional oflag kflag acc)
  (cond ((null params) (reverse acc))
        ((eq (car params) '&rest) (reverse (cons (cdr params) acc)))
        ((eq (car params) '&optional)
         (when kflag (error "&key should not come before &optional"))
         (params2args (cdr params) t kflag acc))
        ((eq (car params) '&key) (params2args (cdr params) oflag t acc))
        ((and kflag (listp (first params)))
         (params2args (cdr params) oflag kflag
                      (append (list (second (first params))
                                    (intern (symbol-name (first (first params))) :keyword)) acc)))
        ((and oflag (listp (first params)))
         (params2args (cdr params) oflag kflag (cons (second (first params)) acc)))
        ((and oflag (null (first params)))
         (params2args (cdr params) oflag kflag (cons nil acc)))
        (t (params2args (cdr params) oflag kflag (cons (first params) acc)))))


(defun get-fname (s)
  (let ((name (symbol-name s)))
    (string-upcase (subseq name 0 (position #\- name :test #'char-equal)))))

(defmacro wrp-defun (defun)
  (unless (listp (third defun))
    (error "Invalid syntax for DEFUN form!"))
  (let* ((name (second defun))
         (params (third defun))
         (bdy (cdddr defun))
         (new-name (gensym (format nil "~a-" (symbol-name name))))
         (new-params (replace-in-expr name new-name params))
         (args (params2args new-params))
         (new-bdy (replace-in-expr name new-name bdy)))
    `(defun ,name (,@new-params)
       (let ((depth 0))
         (labels ((,new-name (,@new-params)
                    (if (> depth *MAX-DEPTH*)
                        (error "Recursion too deep in function ~a !" ,(get-fname new-name))
                        (progn
                          (incf depth)
                          ,@new-bdy))))
           (apply #',new-name (list ,@args)))))))


(defun wrp-load-std-sols (file)
  "This function should be used for loading the student's lisp file.
   It reads the forms in the student's lisp file.  If a form is a DEFUN, uses a macro 
   to rewrite the function to avoid it causing a stack overflow when the function is called, 
   then evals the form. Otherwise, evals the form."
  (with-open-file (in file :direction :input)
    (loop for form = (read in nil nil)
          while form 
          do (eval (if (and (consp form) (eq (car form) 'defun))
                       `(wrp-defun ,form)
                       form)))))

;; (wrp-func '(defun fact (x &optional (acc 1) (if (< x 2) acc (fact (1- x) (* x acc)))))
