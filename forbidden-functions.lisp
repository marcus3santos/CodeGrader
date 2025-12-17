(defpackage :chk-forbidden-functions
  (:use :cl :gensymifier)
  (:export :used-forbidden-function-p))

(in-package :chk-forbidden-functions)

(defun used-forbidden-function-p (q-func-name forbidden-functions student-forms)
  "Takes a function name Q-FUNC-NAME (a symbol), a list of function names FORBIDDEN-FUNCTIONS,
   and a list STUDENT-FORMS containing the forms present in the student's lisp program file. 
  Returns FUNC if Q-FUNC-NAME directly or indirectly calls FUNC and FUNC is in FORBIDDEN-FUNCTIONS."
  (let ((function-table (make-hash-table))
        (global-identifier-table (make-hash-table)))
    ;; Build function and global identifier tables: name → body
    (dolist (form student-forms)
      (let ((form-wth-uniq-vars (gensymify form)))
        (cond ((and (consp form-wth-uniq-vars) (eq (first form-wth-uniq-vars) 'defun))
               (setf (gethash (second form-wth-uniq-vars) function-table)
                     (cdddr form-wth-uniq-vars)))
              ((and (consp form-wth-uniq-vars)
                    (or (eq (first form-wth-uniq-vars) 'defvar)
                        (eq (first form-wth-uniq-vars) 'defparameter)
                        (eq (first form-wth-uniq-vars) 'defconstant)))
               (setf (gethash (second form-wth-uniq-vars) global-identifier-table)
                     (cddr form-wth-uniq-vars))))))
    ;; Depth-first search
    (labels ((function-designator->symbol (fd)
               "Return a symbol if FD statically names a function, else NIL."
               (cond
                 ;; #'foo
                 ((and (consp fd) (eq (car fd) 'function))
                  (cadr fd))
                 ;; 'foo
                 ((and (consp fd) (eq (car fd) 'quote))
                  (cadr fd))
                 ;; bare symbol (e.g., (funcall foo ...))
                 ((symbolp fd)
                  fd)
                 (t nil)))
             (scan (aname fvisited gvvisited)
               (let ((name (function-designator->symbol aname)))
                 (cond
                   ;; direct forbidden call?
                   ((member name forbidden-functions)
                    name)
                   ;; already visited? avoid infinite loops
                   ((or (member name fvisited)
                        (member name gvvisited)) ;; case of a weird naming cycle
                    nil)
                   ;; otherwise look at its body
                   (t
                    (let* ((fbody (gethash name function-table))
                           (fvisited (cons name fvisited))
                           (form (gethash name global-identifier-table))
                           (gvvisited (cons name gvvisited)))
                      (or (when fbody
                            (calls-forbidden-p fbody fvisited gvvisited))
                          (when form
                            (calls-forbidden-p form fvisited gvvisited))))))))
             (calls-forbidden-p (forms fvisited gvvisited)
               (cond
                 ((null forms) nil)
                 ;; Direct call: (foo ....)
                 ((and (symbolp (first forms))
                       (scan (first forms) fvisited gvvisited))
                  (first forms))
                 ;; recur through subforms and rest
                 ((consp (first forms))
                  (calls-forbidden-p (first forms) fvisited gvvisited))
                 ((atom (first forms))
                  (calls-forbidden-p (rest forms) fvisited gvvisited))
                 (t nil))))
      ;; Start with q-func-name
      (scan q-func-name '() '()))))

(defun test ()
  (let ((q-func-name 'caca)
        (forbidden-functions '(caca1 caca2))
        (student-forms '((defparameter v #'caca2)
                         (defvar c v)
                         (defconstant d c)
                         (defun caca5 () (caca2))
                         (defun caca3 () (let ((caca1 (caca5)))))
                         (defun caca () (caca3))
                         )))
    (used-forbidden-function-p q-func-name forbidden-functions student-forms)))
