(defpackage :normalizer
  (:use :cl)
  (:export :normalize :change-vars))

(in-package :normalizer)


(defun form< (a b)
  "Compare two Common Lisp forms A and B.
Returns T if A is considered less than B."
  (cond
    ;; Numbers: compare numerically
    ((and (numberp a) (numberp b))
     (< a b))

    ;; Strings: compare lexicographically
    ((and (stringp a) (stringp b))
     (string< a b))

    ;; Symbols: compare by name lexicographically
    ((and (symbolp a) (symbolp b))
     (string< (symbol-name a) (symbol-name b)))

    ;; Lists: compare element by element recursively
    ((and (listp a) (listp b))
     (cond
       ((null a) (not (null b)))   ; empty list is less than non-empty
       ((null b) nil)           ; non-empty list is greater than empty
       (t (or (form< (first a) (first b))
              (and (equal (first a) (first b))
                   (form< (rest a) (rest b)))))))

    ;; Different types: define some arbitrary type ordering
    (t
     (< (type-order a) (type-order b)))))

(defun type-order (x)
  "Assign an ordering value to a type."
  (cond
    ((numberp x) 0)
    ((stringp x) 1)
    ((symbolp x) 2)
    ((listp x) 3)
    (t 4)))

(defun sort-form (f)
  (cond ((null f) f)
        ((atom f) f)
        ((and (listp f)
              (or (eq (car f) '*)
                  (eq (car f) '+)))
         (cons (car f)
               (sort (mapcar #'sort-form (cdr f)) #'form<)))
        (t (cons (sort-form (car f)) (mapcar #'sort-form (cdr f))))))


(defun encode-form (f symbs)
  "F is a form and SYMBS is a list of pairs (SYMB CODE). Returns a form in which
   each symbol SYMB occurring in F is replaced by the respective CODE."
  (cond ((null f) f)
        ((symbolp f) (cadar (member f symbs :key #'car)))
        ((listp f) (cons (encode-form (car f) symbs) (encode-form (cdr f) symbs)))
        (t f)))

(defun get-symbols (form &optional (type "string"))
  "Returns a list of pairs (SYMB CODE) where SYMB is a symbol occurring in form
   and code is a string denoting a unique code for SYMB"
  (let ((symb-num -1)
        (acc '()))
    (labels ((walk (f)
               (cond
                 ((null f) nil)
                 ((symbolp f)         ; how to deal with quoted forms?
                  (unless (assoc f acc)
                    (incf symb-num)
                    (if (string= type "string")
                        (push (list f (format nil "~a" symb-num)) acc)
                        (push (list f (gensym)) acc))))
                 ((listp f)
                  (walk (car f))
                  (walk (cdr f))))))
      (walk form)
      (nreverse acc))))

(defun variables-in-lambda-list (lambda-list &optional res after-marker (markers '(&optional &key &rest)))
  (cond ((null lambda-list) (reverse res))
        ((member (car lambda-list) markers)
         (variables-in-lambda-list (cdr lambda-list) res t))
        ((symbolp (car lambda-list))
         (variables-in-lambda-list (cdr lambda-list) (cons (car lambda-list) res) after-marker))
        ((and (listp (car lambda-list))
              after-marker
              (symbolp (caar lambda-list)))
         (variables-in-lambda-list (cdr lambda-list) (cons (caar lambda-list) res) t))
        (t (error "Malformed lambda list!"))))

(defun replace-vars-in-bdy (ll bdy env type)
  (let* ((ll-symbs (get-symbols ll type))
         (new-ll (mapcar #'(lambda (x)
                             (change-vars x ll-symbs type))
                         ll)))
    (values new-ll
            (if (listp bdy)
                (mapcar #'(lambda (x)
                            (change-vars x (append ll-symbs env) type))
                        bdy)
                (change-vars bdy (append ll-symbs env) type)))))

(defun change-vars (f  &optional symbs-codes (type "string"))
  "Changes each variable occurring in form F to its respective code obtained
   from the list of symb-code pairs in SYMBS-CODES"
  (format t "~s~%" symbs-codes)
  (cond ((null f) f)
        ((symbolp f) (let ((sc (assoc f symbs-codes)))
                       (if sc (cadr sc) f)))
        ((listp f)
         (let ((op (car f)))
           (cond
             ((or (and (eql op 'defun)  ; DEFUN, LAMBDA
                       (symbolp (cadr f))
                       (listp (caddr f)))
                  (and (eql op 'lambda)
                       (listp (second f))))
              (let* ((lambda-list (if (eql op 'defun)
                                      (variables-in-lambda-list (third f))
                                      (variables-in-lambda-list (second f))))
                     (bdy (if (eql op 'defun)
                              (cdddr f)
                              (cddr f))))
                (multiple-value-bind (new-ll new-bdy) (replace-vars-in-bdy lambda-list bdy symbs-codes type)
                  (if (eql op 'defun)
                      `(defun ,(second f) ,new-ll
                         ,@new-bdy)
                      `(lambda ,new-ll ,@new-bdy)))))
             ((and (or (eq op 'labels) (eq op 'flet) (eq op 'macrolet)) ; LABELS, FLET, MACROLET
                   (listp (second f)))
              (let ((defs (second f))
                    (bdy (cddr f)))
                `(,op ,(mapcar (lambda (def)
                                 (let* ((fname (first def))
                                        (lambda-list (variables-in-lambda-list (second def))))
                                   (multiple-value-bind (new-ll new-bdy) (replace-vars-in-bdy lambda-list (cddr def) symbs-codes type)
                                     `(,fname ,new-ll
                                              ,@new-bdy))))
                               defs)
                      ,@(mapcar (lambda (x)
                                  (change-vars x symbs-codes type))
                                bdy))))
             ((and (eq op 'multiple-value-bind) ; MULTIPLE-VALUE-BIND
                   (listp (second f))
                   (third f))
              (let* ((lambda-list (variables-in-lambda-list (cadr f))))
                (multiple-value-bind (new-ll new-bdy) (replace-vars-in-bdy lambda-list (cdddr f) symbs-codes type)
                  `(multiple-value-bind ,new-ll ,(change-vars (caddr f) symbs-codes type)
                     ,@new-bdy))))
             ((and (or (eq op 'let) (eq op 'let*)
                       (and (or (eq op 'do) (eq op 'do*))
                            (consp (third f))) ;; checking syntax
                       (and (eq op 'dotimes)
                            (symbolp (first (second f)))))
                   (listp (second f)))  ; LET, LET*, DO, DO*, DOTIMES
              (let* ((lvars (if (eql op 'dotimes)
                                (list (first (second f)))
                                (mapcar (lambda (x)
                                          (if (listp x) (first x) x))
                                        (second f)))))
                (multiple-value-bind (_ new-bdy)
                    (if (or (eq op 'do) (eq op 'do*))
                        (replace-vars-in-bdy lvars (cdddr f) symbs-codes type)
                        (replace-vars-in-bdy lvars (cddr f) symbs-codes type))
                  (declare (ignore _))
                  `(,op
                    ,(if (eql op 'dotimes)
                         (mapcar (lambda (x)
                                   (multiple-value-bind (_ new-exp) (replace-vars-in-bdy lvars x symbs-codes type)
                                     (declare (ignore _))
                                     new-exp))
                                 (second f))
                         (mapcar (lambda (x)
                                   (if (listp x) ; takes care of bindings of the kind ((x 1) y (z 2) ...)
                                       `(,(multiple-value-bind (_ new-var) (replace-vars-in-bdy lvars (car x) symbs-codes type)
                                            (declare (ignore _))
                                            new-var)
                                         ,@(multiple-value-bind (_ new-exp) (replace-vars-in-bdy lvars (cdr x) symbs-codes type)
                                             (declare (ignore _))
                                             new-exp))
                                       `(,(multiple-value-bind (_ new-var) (replace-vars-in-bdy lvars x symbs-codes type)
                                            (declare (ignore _))
                                            new-var) nil)))
                                 (second f)))
                    
                    ,@(if (or (eq op 'do) (eq op 'do*))
                          (append (list (mapcar (lambda (x)
                                                  (multiple-value-bind (_ new-exp) (replace-vars-in-bdy lvars x symbs-codes type)
                                                    (declare (ignore _))
                                                    new-exp))
                                                (third f)))
                                  new-bdy)
                          new-bdy)))))
             (t (append  (list (car f))
                         (mapcar #'(lambda (f)
                                     (change-vars f symbs-codes type))
                                 (cdr f)))))))
        (t f)))


(defun normalize (f)
  (sort-form (change-vars (macroexpand f))))
