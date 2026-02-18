(defpackage :normalizer
  (:use :cl)
  (:export :normalize))

(in-package :normalizer)

(defun type-order (x)
  "Assign an ordering value to a type."
  (cond
    ((numberp x) 0)
    ((stringp x) 1)
    ((symbolp x) 2)
    ((listp x) 3)
    (t 4)))

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
