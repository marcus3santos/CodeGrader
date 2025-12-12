(defun forbidden-symbols-present-p (fname form forbidden-symbols student-functions call-chain)
  (cond ((null form) nil)
        ((and (listp form) ; form is a defun
              (eq (first form) 'defun))
         (or (forbidden-symbols-present-p fname (third form) forbidden-symbols student-functions call-chain)  
             (forbidden-symbols-present-p fname (nthcdr 3 form) forbidden-symbols student-functions call-chain)))
        ((and (listp form) ;; a direct or indirect  recursive function call
              (or (eq (first form) fname)
                  (member (first form) call-chain)))
         (let (res)
           (dolist (aform (rest form) res)
             (setf res (forbidden-symbols-present-p fname aform forbidden-symbols student-functions call-chain))
             (when res (return res)))))
        ((listp form)    ;; a function call
         (or (forbidden-symbols-present-p fname (first form) forbidden-symbols student-functions call-chain) ; of a forbidden function
             (let (res)  ; its arguments call a forbidden function
               (dolist (aform (rest form) res)
                 (setf res (forbidden-symbols-present-p fname aform forbidden-symbols student-functions call-chain))
                 (when res (return res))))
             (used-forbidden-symbolp (first form) forbidden-symbols student-functions) call-chain)) ; indirectly calls a forbidden functionm
        ((symbolp form) (member form forbidden-symbols))))

(defun used-forbidden-symbolp (q-func-name forbidden-symbols student-functions &optional call-chain)
  "Returns T if q-func-name is defined in student-functions and does not 
   use any of the forbidden-functions in its body or its helpers body."
  (let* ((res (member q-func-name student-functions
                      :key (lambda (x)
                             (if (atom x) x (second x)))))
         (func-defn (first res)))
    (if func-defn
        (forbidden-symbols-present-p q-func-name func-defn forbidden-symbols student-functions (cons q-func-name call-chain))
        (error "Undefined function: ~%~T~a" q-func-name))))
