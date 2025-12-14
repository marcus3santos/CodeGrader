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
    (labels ((scan (fname visited )
               (cond
                 ;; direct forbidden call?
                 ((member fname forbidden-functions)
                  t)
                 ;; already visited? avoid infinite loops
                 ((member fname visited)
                  nil)
                 ;; otherwise look at its body
                 (t
                  (let* ((body (gethash fname function-table))
                         (visited (cons fname visited)))
                    (when body
                      (calls-forbidden-p body visited))))))
             (calls-forbidden-p (forms visited)
               (cond
                 ((null forms) nil)
                 ((and (symbolp (first forms))
                       (scan (first forms) visited))
                  t)                 
                 ;; recur through subforms and rest
                 ((or (and (consp (first forms))
                           (calls-forbidden-p (first forms) visited))
                      (calls-forbidden-p (rest forms) visited))
                  t)
                 (t nil))))
      ;; Start with q-func-name
      (scan q-func-name '()))))
