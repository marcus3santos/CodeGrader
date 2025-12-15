(defpackage :chk-forbidden-functions
  (:use :cl :gensymifier)
  (:export :used-forbidden-function-p))

(in-package :chk-forbidden-functions)

(defun used-forbidden-function-p (q-func-name forbidden-functions student-forms)
  "Takes a function name Q-FUNC-NAME (a symbol), a list of function names FORBIDDEN-FUNCTIONS,
   and a list STUDENT-FORMS containing the forms present in the student's lisp program file. 
  Returns T if Q-FUNC-NAME directly or indirectly calls a function in FORBIDDEN-FUNCTIONS."
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
    (maphash (lambda (k  v) (format t "function --- ~s ~s~%" k v)) function-table)
    (maphash (lambda (k  v) (format t "Global id --- ~s ~s~%" k v)) global-identifier-table )
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
             #|
             (mentions-forbidden-p (form forbidden)
               (let ((func (function-designator->symbol form)))
                 (cond
                   ((member func forbidden)
                    t)
                   ((atom form)
                    nil)
                   ((eq (car form) 'quote)
                    nil)
                   (t
                    (or (mentions-forbidden-p (car form) forbidden)
                        (mentions-forbidden-p (cdr form) forbidden))))))
             |#
             (scan (aname fvisited gvvisited)
               (let ((name (function-designator->symbol aname)))
                 (cond
                   ;; direct forbidden call?
                   ((member name forbidden-functions)
                    t)
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
                      (when fbody
                        (calls-forbidden-p fbody fvisited gvvisited))
                      (when form
                        (calls-forbidden-p form fvisited gvvisited)))))))
             (calls-forbidden-p (forms fvisited gvvisited)
               (format t "Entered forms: ~s~%" forms)
               (cond
                 ((null forms) nil)
                 ;; Direct call: (foo ....)
                 ((and (symbolp (first forms))
                       (scan (first forms) fvisited gvvisited))
                  t)
                 ;; funcall / apply
                 #|
                 ((and (symbolp (first forms)) ;
                 (member (first forms) '(funcall apply))) ;
                 (let ((fn (function-designator->symbol (second forms)))) ;
                 (or (and fn (scan fn visited)) ;
                        ;; still recurse through arguments ;
                 (calls-forbidden-p (rest forms) visited)))) ;
                                        ;
                 ;; Mentions forbidden function ;
                 ((mentions-forbidden-p forms forbidden-functions) ;
                 t)|#
                 ;; recur through subforms and rest
                 ((or (and (consp (first forms))
                           (calls-forbidden-p (first forms) fvisited gvvisited))
                      (calls-forbidden-p (rest forms) fvisited gvvisited))
                  (format t "--- First forms: ~s~%Rest forms: ~s~%" (first forms) (rest forms))
                  t)
                 (t nil))))
      ;; Start with q-func-name
      (scan q-func-name '() '()))))

(defun test ()
  (let ((q-func-name 'caca)
        (forbidden-functions '(caca1 caca2))
        (student-forms '((defparameter v #'caca2)
                         (defun caca3 () (funcall #'caca1))
                         (defun caca () (funcall v))
                         )))
    (used-forbidden-function-p q-func-name forbidden-functions student-forms)))
