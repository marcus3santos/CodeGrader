(defpackage :gensymifier
  (:use :cl )
  (:export :gensymify))

(in-package :gensymifier)

(defun gensymify (form &optional venv fenv)
  "Alpha-convert FORM by renaming all lexically bound variables to fresh GENSYMs.

GENSYMIFY walks a Common Lisp form and replaces each lexical binding
with a uniquely generated symbol, preserving lexical scope and
shadowing. All variable references in value position are rewritten to
refer to the corresponding gensym.

Handled binding forms include:
  DEFUN, LAMBDA
  LET, LET*
  FLET, LABELS
  DO, DO*
  DOTIMES
  MULTIPLE-VALUE-BIND

Initialization forms are evaluated in the correct (outer) environment,
while bodies are walked in the extended environment. Sequential binding
semantics of LET*, DO*, and LABELS are respected.

Quoted forms are left untouched. Function names and operators are not
renamed at call sites; only lexically bound variables are affected.

This function is intended for hygienic macro expansion and program
transformation, not for evaluation. It does not handle declarations,
special variables, SYMBOL-MACROLET, MACROLET, SETF expanders, or
compiler macros. Use after MACROEXPAND-ALL for best results."
  (labels
      ((vlookup (s venv)  (or (cdr (assoc s venv)) s))
       (flookup (s fenv) (or (cdr (assoc s fenv)) s))

       (walk (f venv fenv)
         (cond
           ;; Symbols
           ((symbolp f)
            (vlookup f venv))

           ;; Quoted forms
           ((and (consp f) (eq (car f) 'quote))
            f)

           ;; Lambda
           ((and (consp f) (eq (car f) 'lambda))
            (destructuring-bind (lambda params &rest body) f
              (declare (ignore lambda))
              (let* ((new (mapcar (lambda (p)
                                    (cons p (gensym (symbol-name p))))
                                  params))
                     (venv* (append new venv)))
                `(lambda ,(mapcar #'cdr new)
                   ,@(mapcar (lambda (b) (walk b venv* fenv)) body)))))

           ;; DEFUN
           ((and (consp f) (eq (car f) 'defun))
            (destructuring-bind (defun name params &rest body) f
              (declare (ignore defun))
              (let* ((new (mapcar (lambda (p)
                                    (cons p (gensym (symbol-name p))))
                                  params))
                     (venv* (append new venv)))
                `(defun ,name ,(mapcar #'cdr new)
                   ,@(mapcar (lambda (b) (walk b venv* fenv)) body)))))
           ;; LET 
           ((and (consp f) (eq (car f) 'let))
            (let* ((bindings (cadr f))
                   (body (cddr f))
                   (new (mapcar (lambda (b)
                                  (cons (if (consp b) (car b) b)
                                        (gensym (symbol-name (if (consp b) (car b) b)))))
                                bindings))
                   (venv* (append new venv)))
              `(let
                   ,(mapcar (lambda (b)
                              `(,(cdr (assoc (if (consp b) (car b) b) new))
                                ,(walk (if (consp b) (cadr b) nil) venv fenv)))
                     bindings)
                 ,@(mapcar (lambda (b)
                             (walk b venv* fenv))   
                           body))))

           ;; LET*
           ((and (consp f) (eq (car f) 'let*))
            (let ((bindings (cadr f))
                  (body (cddr f)))
              (labels ((walk-bindings (bs venv acc)
                         (if (null bs)
                             (values (nreverse acc) venv)
                             (let* ((binding (if (consp (car bs))
                                                 (car bs)
                                                 (list (car bs))))
                                    (var (car binding))
                                    (init (cadr binding))
                                    (g (gensym (symbol-name var)))
                                    (init* (walk init venv fenv))
                                    (venv* (acons var g venv)))
                               (walk-bindings (cdr bs)
                                              venv*
                                              (cons `(,g ,init*) acc))))))
                (multiple-value-bind (new-bindings venv*)
                    (walk-bindings bindings venv '())
                  `(let* ,new-bindings
                     ,@(mapcar (lambda (b) (walk b venv* fenv)) body))))))

           ;; FLET / LABELS
           ((and (consp f) (member (car f) '(flet labels)))
            (let* ((kind (car f))
                   (defs (cadr f))
                   (body (cddr f))
                   (fnew (mapcar (lambda (d)
                                   (cons (car d)
                                         (gensym (symbol-name (car d)))))
                                 defs))
                   (fenv* (append fnew fenv)))
              `(,kind
                ,(mapcar
                  (lambda (d)
                    (destructuring-bind (name params &rest fbody) d
                      (let* ((pnew (mapcar (lambda (p)
                                             (cons p (gensym (symbol-name p))))
                                           params))
                             (venv* (append pnew venv)))
                        `(,(cdr (assoc name fnew))
                          ,(mapcar #'cdr pnew)
                          ,@(mapcar (lambda (b)
                                      (walk b venv* fenv*))
                                    fbody)))))
                  defs)
                ,@(mapcar (lambda (b) (walk b venv fenv*)) body))))

           ;; DO 
           ((and (consp f) (eq (car f) 'do))
            (destructuring-bind (do vars (test &rest results) &rest body) f
              (declare (ignore do))
              (let* ((new (mapcar (lambda (v)
                                    (cons (car v)
                                          (gensym (symbol-name (car v)))))
                                  vars))
                     (venv* (append new venv)))
                `(do
                  ,(mapcar
                    (lambda (v)
                      (destructuring-bind (var init &optional step) v
                        `(,(vlookup var venv*) 
                          ,(walk init venv fenv)
                          ,(walk step venv* fenv))))
                    vars)
                  (,(walk test venv* fenv)
                   ,@(mapcar (lambda (r) (walk r venv* fenv)) results))
                   ,@(mapcar (lambda (b) (walk b venv* fenv)) body)))))
           ;; DO*
           ((and (consp f) (eq (car f) 'do*))
            (destructuring-bind (_ vars (test &rest results) &rest body) f
              (declare (ignore _))
              (let ((venv* venv)
                    (new-vars '()))
                ;; sequential variable introduction
                (dolist (v vars)
                  (let* ((var (car v))
                         (init (cadr v))
                         (step (caddr v))
                         (g (gensym (symbol-name var)))
                         (init* (walk init venv* fenv)))
                    (setf venv* (acons var g venv*))
                    (push `(,g ,init* ,(walk step venv* fenv)) new-vars)))
                `(do* ,(nreverse new-vars)
                      (,(walk test venv* fenv)
                       ,@(mapcar (lambda (r) (walk r venv* fenv)) results))
                   ,@(mapcar (lambda (b) (walk b venv* fenv)) body)))))

           ;; DOTIMES
           ((and (consp f) (eq (car f) 'dotimes))
            (destructuring-bind (dotimes (var count &optional result) &rest body) f
              (declare (ignore dotimes))
              (let* ((g (gensym (symbol-name var)))
                     (venv* (acons var g venv)))
                `(dotimes (,g ,(walk count venv fenv)
                              ,(walk result venv* fenv))
                   ,@(mapcar (lambda (b) (walk b venv* fenv)) body)))))

           ;; MULTIPLE-VALUE-BIND
           ((and (consp f) (eq (car f) 'multiple-value-bind))
            (destructuring-bind (mvb vars expr &rest body) f
              (declare (ignore mvb))
              (let* ((new (mapcar (lambda (v)
                                    (cons v (gensym (symbol-name v))))
                                  vars))
                     (venv* (append new venv)))
                `(multiple-value-bind
                       ,(mapcar #'cdr new)
                     ,(walk expr venv fenv)
                   ,@(mapcar (lambda (b) (walk b venv* fenv)) body)))))

           ;; Function call or general form
           ((consp f)
            (let ((head (car f)))
              (cons (if (symbolp head) (flookup head fenv) head)
                    (mapcar (lambda (x)
                              (if (symbolp x)
                                  (vlookup x venv)
                                  (walk x venv fenv)))
                            (cdr f)))))

           (t f))))
    (walk form venv fenv)))

(defun normalize-gensyms (form)
  "Replace all gensyms with deterministic symbols G1, G2, ..."
  (let ((table (make-hash-table :test #'eq))
        (counter 0))
    (labels ((norm (x)
               (cond
                 ((and (symbolp x) (null (symbol-package x)))
                  (or (gethash x table)
                      (setf (gethash x table)
                            (intern (format nil "G~D" (incf counter))))))
                 ((consp x)
                  (cons (norm (car x))
                        (norm (cdr x))))
                 (t x))))
      (norm form))))
