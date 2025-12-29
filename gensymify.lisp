(in-package :gensymifier)

(defun gensymify (form &optional venv fenv)
  "Alpha-convert FORM by renaming all lexically bound variables to fresh GENSYMs.
Handles complex lambda lists for DEFUN, LAMBDA, FLET, and LABELS."
  (labels
      ((vlookup (s venv)  (or (cdr (assoc s venv)) s))
       (flookup (s fenv) (or (cdr (assoc s fenv)) s))

       ;; Processes a lambda-list and returns (values new-lambda-list updated-venv)
       (walk-lambda-list (lambda-list venv fenv)
         (let ((new-venv venv)
               (new-ll '()))
           (dolist (item lambda-list)
             (cond
               ((member item '(&optional &key &rest &aux &allow-other-keys &whole &environment))
                (push item new-ll))
               ((symbolp item)
                (let ((g (gensym (symbol-name item))))
                  (setf new-venv (acons item g new-venv))
                  (push g new-ll)))
               ((consp item)
                (let* ((var-part (car item))
                       ;; Handle (&key ((:k var) init sp)) vs (var init sp)
                       (var (if (consp var-part) (cadr var-part) var-part))
                       (key-sym (if (consp var-part) (car var-part) nil))
                       (init (cadr item))
                       (sp (caddr item))
                       (g-var (gensym (symbol-name var)))
                       (g-sp (when sp (gensym (symbol-name sp))))
                       ;; Walk init in the venv PRIOR to this binding (sequential-ish)
                       (init* (walk init new-venv fenv)))
                  (setf new-venv (acons var g-var new-venv))
                  (when g-sp (setf new-venv (acons sp g-sp new-venv)))
                  (push (if g-sp
                            (list (if key-sym (list key-sym g-var) g-var) init* g-sp)
                            (list (if key-sym (list key-sym g-var) g-var) init*))
                        new-ll)))
               (t (push item new-ll))))
           (values (nreverse new-ll) new-venv)))

       (walk (f venv fenv)
         (cond
           ((symbolp f) (vlookup f venv))
           ((and (consp f) (eq (car f) 'quote)) f)

           ;; LAMBDA
           ((and (consp f) (eq (car f) 'lambda)
                 (listp (second f)))
            (handler-case
                (destructuring-bind (head params &rest body) f
                  (multiple-value-bind (new-params venv*) (walk-lambda-list params venv fenv)
                    `(,head ,new-params
                            ,@(mapcar (lambda (b) (walk b venv* fenv)) body))))
              (error () f)))

           ;; DEFUN
           ((and (consp f) (eq (car f) 'defun)
                 (listp (third f)))
            (handler-case
                (destructuring-bind (head name params &rest body) f
                  (multiple-value-bind (new-params venv*) (walk-lambda-list params venv fenv)
                    `(,head ,name ,new-params
                            ,@(mapcar (lambda (b) (walk b venv* fenv)) body))))
              (error () f)))

           ;; FLET / LABELS
           ((and (consp f) (member (car f) '(flet labels)))
            (handler-case
                (let* ((kind (car f))
                       (defs (cadr f))
                       (body (cddr f))
                       (fnew (mapcar (lambda (d) (cons (car d) (gensym (symbol-name (car d))))) defs))
                       (fenv* (append fnew fenv)))
                  `(,kind
                    ,(mapcar
                      (lambda (d)
                        (destructuring-bind (name params &rest fbody) d
                          (multiple-value-bind (new-params venv*) 
                              (walk-lambda-list params venv (if (eq kind 'labels) fenv* fenv))
                            `(,(flookup name fnew) ,new-params
                              ,@(mapcar (lambda (b) (walk b venv* fenv*)) fbody)))))
                      defs)
                    ,@(mapcar (lambda (b) (walk b venv fenv*)) body)))
              (error () f)))

           ;; LET / LET* 
           ((and (consp f) (member (car f) '(let let*)))
            (handler-case 
                (let ((is-star (eq (car f) 'let*))
                      (bindings (cadr f))
                      (body (cddr f))
                      (new-bindings '())
                      (venv-for-body venv))
                  (dolist (b bindings)
                    (let* ((var (if (consp b) (car b) b))
                           (init (if (consp b) (cadr b) nil))
                           (g (gensym (symbol-name var)))
                           (init* (walk init (if is-star venv-for-body venv) fenv)))
                      (setf venv-for-body (acons var g venv-for-body))
                      (push `(,g ,init*) new-bindings)))
                  `(,(car f) ,(nreverse new-bindings)
                    ,@(mapcar (lambda (b) (walk b venv-for-body fenv)) body)))
              (error () f)))

           ;; Other forms (DO, DOTIMES, etc. remain the same as your source)

           ;; DO / DO*
           ((and (consp f) (member (car f) '(do do*)))
            (handler-case 
                (destructuring-bind (kind vars (test &rest results) &rest body) f
                  (let* ((new (mapcar (lambda (v)
                                        (cons (car v)
                                              (gensym (symbol-name (car v)))))
                                      vars))
                         (venv* (append new venv)))
                    `(,kind
                      ,(mapcar
                        (lambda (v)
                          (cond ((null v) v)
                                ((and (listp v)
                                      (= (length v) 2))
                                 (destructuring-bind (var init) v
                                   `(,(cdr (assoc var new))
                                     ,(walk init venv fenv))))
                                ((and (listp v)
                                      (= (length v) 3))
                                 (destructuring-bind (var init step) v
                                   `(,(cdr (assoc var new))
                                     ,(walk init venv fenv)
                                     ,(walk step venv* fenv))))
                                (t v)))
                        vars)
                      (,(walk test venv* fenv)
                       ,@(mapcar (lambda (r) (walk r venv* fenv)) results))
                      ,@(mapcar (lambda (b) (walk b venv* fenv)) body))))
              (error () f)))

           ;; DOTIMES
           ((and (consp f) (eq (car f) 'dotimes))
            (handler-case
                (destructuring-bind (dotimes (var count &optional result) &rest body) f
                  (declare (ignore dotimes))
                  (let* ((g (gensym (symbol-name var)))
                         (venv* (acons var g venv)))
                    `(dotimes (,g ,(walk count venv fenv)
                                  ,(walk result venv* fenv))
                       ,@(mapcar (lambda (b) (walk b venv* fenv)) body))))
              (error () f)))
           
           ;; MULTIPLE-VALUE-BIND
           ((and (consp f) (eq (car f) 'multiple-value-bind))
            (handler-case
                (destructuring-bind (mvb vars expr &rest body) f
                  (let* ((new (mapcar (lambda (v) (cons v (gensym (symbol-name v)))) vars))
                         (venv* (append new venv)))
                    `(,mvb ,(mapcar #'cdr new) ,(walk expr venv fenv)
                           ,@(mapcar (lambda (b) (walk b venv* fenv)) body))))
              (error () f)))
           ((and (listp f) (consp (car f)))
            (cons (gensymify (car f))
                  (gensymify (cdr f))))
           ((consp f)
            (let ((head (car f)))
              (cons (if (symbolp head) (flookup head fenv) head)
                    (mapcar (lambda (x) (walk x venv fenv)) (cdr f)))))
           (t f))))
    (walk form venv fenv)))

(defun normalize-gensyms (form)
  "Replace all gensyms with deterministic symbols G1, G2, ...
   and normalize equivalent forms, first <-> car, ..."
  (let ((table (make-hash-table :test #'eq))
        (counter 0))
    (labels ((norm (x)
               (cond
                 ((and (symbolp x) (null (symbol-package x)))
                  (or (gethash x table)
                      (setf (gethash x table)
                            (intern (format nil "G~D" (incf counter))))))
                 ((and (consp x) ; open-branch if a
                       (eq 'if (first x))
                       (null (cdddr x)))
                  (list 'when (norm (second x)) (norm (third x))))
                 ((consp x)
                  (cons (norm (car x))
                        (norm (cdr x))))
                 ((symbolp x)
                  (or (and (eq 'car x) 'first)
                      (and (eq 'cdr x) 'rest)
                      (and (eq 'cadr x) 'second)
                      (and (eq 'caddr x) 'third)
                      x))
                 (t x))))
      (norm form))))
