(defun used-forbidden-function-p (q-func-name forbidden-functions student-functions)
  "Return T if Q-FUNC-NAME directly or indirectly calls a function in FORBIDDEN-FUNCTIONS.
STUDENT-FUNCTIONS is a list of DEFUN forms as read from the student’s file."
  (let ((function-table (make-hash-table)))
    ;; Build function table: name → body
    (dolist (form student-functions)
      (when (and (consp form) (eq (first form) 'defun))
        (setf (gethash (second form) function-table)
              (cdddr form))))        ; skip DEFUN, name, arglist
    ;; Depth-first search
    (labels ((scan (fname visited)
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
                 ;; lambda form
                 ((and (consp forms)
                       (or (eq (first forms) 'lambda)
                           (eq (first forms) '#'lambda)))
                  (calls-forbidden-p (cddr forms) visited))
                 ;; let, let*, do, do* forms
                 ((or (eq (first forms) 'let)
                      (eq (first forms) 'let*)
                      (eq (first forms) 'do)
                      (eq (first forms) 'do*)
                      (eq (first forms) 'dotimes))
                  (or
                   (calls-forbidden-p (mapcar #'rest (second forms)) visited)
                   (calls-forbidden-p (cddr forms) visited)))
                 ;; recur through subforms and rest
                 ((or (and (consp (first forms))
                           (calls-forbidden-p (first forms) visited))
                      (calls-forbidden-p (rest forms) visited))
                  t)
                 (t nil))))
      ;; Start with q-func-name
      (scan q-func-name '()))))
