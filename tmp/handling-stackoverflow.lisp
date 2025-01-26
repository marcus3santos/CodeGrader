(defun replace-in-expr (s ns expr)
  (cond
    ((eq expr s) ns)
    ((listp expr)
     (cons (replace-in-expr s ns (car expr))  
           (mapcar (lambda (item) (replace-in-expr s ns item)) (cdr expr)))) 
    (t expr)))

#|
(defun params2args (args &optional oflag kflag acc)
  (cond ((null args) (reverse acc))
        ((eq (car args) '&rest) (cons (cdr args) acc))
        ((eq (car args) '&optional)
         (when kflag (error "&key should not come before &optional"))
         (params2args (cdr args) t kflag acc))
        ((eq (car args) '&key) (params2args (cdr args) oflag t acc))
        (oflag )))
|#

(defun get-fname (s)
  (let ((name (symbol-name s)))
    (string-upcase (subseq name 0 (position #\- name :test #'char-equal)))))

(defun wrp-func (name args bdy)
  (unless (listp args)
    (error "Invalid syntax for DEFUN form!"))
  (let* ((new-name (gensym (format nil "~a-" (symbol-name name))))
         (new-args (replace-in-expr name new-name args))
         (new-bdy (replace-in-expr name new-name bdy)))
    `(defun ,name (,@new-args)
       (let ((depth 0))
         (labels ((,new-name (,@new-args)
                    (if (> depth *MAX-DEPTH*)
                        (error "Recursion too deep in function ~a !" ,(get-fname new-name))
                        (progn
                          (incf depth)
                          ,@new-bdy))))
           (apply #',new-name (list ,@new-args)))))))


;; (wrp-func 'fact '(x &optional (acc 1)) '((if (< x 2) acc (fact (1- x) (* x acc)))))
