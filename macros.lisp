;; macros.lisp

(in-package :test-runtime)

;; Unit test macros

(defparameter *results* nil)

(defvar *runtime-error* nil)

(defvar *load-error* nil)

(defvar *cr-warning* nil)

(defparameter *forbidden-symbols* nil) ;; List containing the names of the symbols students are not allowed to use

(defparameter *penalty-forbidden* 0.5) ;; Penalty (to be multiplied by total lab mark) for using forbidden symbols

(defparameter *test-name* nil)

; Defines the max depth of recursion for functions

(defparameter *max-rec-depth* 20000)


;; Maximum running time (in seconds) allotted to the
;; evaluation of a test case. Once that time expires, the respective
;; thread is terminated, and a fail is associated to the respective
;; test case 

(defvar *max-time* 0.1) 


(defun report-result (result form)
  (let ((res (not (or (eq result 'runtime-error)
                      (equalp result "runtime-error")
		      (typep result 'condition)
		      (not result)))))
    (push (list res result *test-name* form) *results*)
    ;; (format t "~:[FAIL~;pass~] ...~a: ~a~%" result *test-name* form)
    res))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;; Added to handle detection of endless loop when evaluating test cases
(defmacro time-execution  (expr maxtime)
  "Evaluates expr in a separate thread. 
   If expr's execution time reaches maxtime seconds, then kills the thread and
   pushes the expression that ran out of execution time in *RUNTIME-ERROR*
   then returns *RUNTIME-ERROR*. Otherwise returns the result of evaluating expr."
  (let ((thread (gensym))
	(keep-time (gensym))
	(stime (gensym))
	(res (gensym)))
    `(let* ((,res nil)
	    (,thread (sb-thread:make-thread 
		     (lambda () (setf ,res ,expr)))))
       (labels ((,keep-time (,stime)
		  (cond ((and (> (/ (- (get-internal-real-time) ,stime) 
				    internal-time-units-per-second)
				 ,maxtime)
			      (sb-thread:thread-alive-p ,thread))
			 (progn
			   (sb-thread:terminate-thread ,thread)
			   (push ',(cadr expr) *runtime-error*)
			   (setf ,res 'runtime-error)))
			 ((sb-thread:thread-alive-p ,thread) (,keep-time ,stime))
			 (t ,res))))
	 (,keep-time (get-internal-real-time))))))


(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect
	     `(report-result (time-execution
			      (handler-case ,f
				(error (condition)
				  (push condition *runtime-error*)
				  condition))
			      ,*max-time*) ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (setf *test-name* ',name)
     ,@body))

#|
(let ((*test-name* ',name))   ;(append *test-na
  me* (list ',name))
       ,@body)))
|#

(defun forbidden-symbols (&key (penalty .5) symbols)
  (progn 
    (setf *penalty-forbidden* penalty)
    (setf *forbidden-symbols* symbols)))


;; ---------------------------

(defun generate-unique-name (base-name counter)
  (intern (format nil "~A-~A" base-name counter)))

(defun wfb (binding)
  (every #'identity (mapcar #'(lambda (x) (or (= (length x) 1) (= (length x) 2))) binding)))

(defun rename-vars (expr &optional (env nil) (counter 0))
  "Renames local variables created by Lisp built-in functions/macros
   It cannot handle the syntax of the following functions/macros:
   loop, destructuring-bind, macrolet, catch, and progv"
  (cond
    ;; If the expression is a quoted expression, leave it as is
    ((and (listp expr) (eq (first expr) 'quote)) expr)
    ;; If the expression is a symbol, check if it's a variable that needs renaming
    ((symbolp expr)
     (or (cdr (assoc expr env)) expr))
    ;; If the expression is a LAMBDA form, rename its arguments and body
    ((and (consp expr) (eq (first expr) 'lambda))
     (let* ((args (if (listp (second expr)) ; Syntax checking
                      (second expr)
                      (error "Invalid lambda expression: ~a" expr)))
            (new-args (mapcar (lambda (arg)
                                (generate-unique-name arg (incf counter)))
                              args))
            (new-env (append (mapcar #'(lambda (x y) (cons x y)) args new-args) env)))
       `(lambda ,new-args ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cddr expr)))))
    ;; If the expression is a DEFUN form, rename its arguments and body
    ((and (consp expr) (eq (first expr) 'defun))
     (let* ((args (if (and (symbolp (second expr)) (listp (third expr))) ; Syntax checking
                      (third expr)
                      (error "Invalid defun expression: ~a" expr)))
            (new-args (mapcar (lambda (arg)
                                (generate-unique-name arg (incf counter)))
                              args))
            (new-env (append (mapcar #'(lambda (x y) (cons x y)) args new-args) env)))
       `(defun ,(second expr) ,new-args ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cdddr expr)))))
    
    ;; If the expression is a LET form, rename its bindings and body
    ((and (consp expr) (eq (first expr) 'let))
     (let* ((bindings (if (wfb (second expr)) ; syntax checking
                          (second expr)
                          (error "Invalid binding in expression: ~a" expr)))
            (new-bindings (mapcar (lambda (binding)
                                    (let ((var (if (symbolp binding) binding (first binding)))
                                          (init (if (symbolp binding) nil (second binding))))
                                      (list (generate-unique-name var (incf counter))
                                            (rename-vars init env counter))))
                                  bindings))
            (new-env (append (mapcar #'(lambda (x y) (cons (car x) (car y))) bindings new-bindings) env)))
       `(let ,new-bindings ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cddr expr)))))
    ;; If the expression is a LET* form, rename its bindings and body
    ((and (consp expr) (eq (first expr) 'let*))
     (unless (wfb (second expr))        ; syntax checking
       (error "Invalid binding in expression: ~a" expr))
     (let ((new-env env)
           (new-bindings nil))
       (dolist (binding (second expr))
         (let ((var (if (symbolp binding) binding (first binding)))
               (init (if (symbolp binding) nil (second binding))))
           (let ((new-var (generate-unique-name var (incf counter))))
             (push (list new-var (rename-vars init new-env counter)) new-bindings)
             (push (cons var new-var) new-env))))
       `(let* ,(reverse new-bindings)
          ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cddr expr)))))
    ;; If the expression is a DOTIMES form, rename its variable and body
    ((and (consp expr) (eq (first expr) 'dotimes))
     (unless (let ((var (first (second expr)))) ; Syntax checking
               (and (symbolp var) (or (= (length (second expr)) 2) (= (length (second expr)) 3))))
       (error "Invalid initialization in DOTIMES expression: ~a" expr))
     (let* ((var (first (second expr)))
            (new-var (generate-unique-name var (incf counter)))
            (new-env (cons (cons var new-var) env)))
       `(dotimes (,(rename-vars (first (second expr)) new-env counter) ,(rename-vars (second (second expr)) env counter) ,(rename-vars (third (second expr)) new-env counter))
          ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cddr expr)))))
    ;; If the expression is a DOLIST form, rename its variable and body
    ((and (consp expr) (eq (first expr) 'dolist))
     (unless (let ((var (first (second expr)))) ; Syntax checking
               (and (symbolp var) (or (= (length (second expr)) 2) (= (length (second expr)) 3))))
       (error "Invalid initialization in DOTIMES expression: ~a" expr))
     (let* ((var (first (second expr)))
            (new-var (generate-unique-name var (incf counter)))
            (new-env (cons (cons var new-var) env)))
       `(dolist (,(rename-vars (first (second expr)) new-env counter) ,(rename-vars (second (second expr)) env counter) ,(rename-vars (third (second expr)) new-env counter))
          ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cddr expr)))))
    ;; If the expression is a DO form, rename its variable bindings and body
    ((and (consp expr) (eq (first expr) 'do))
     (let* ((bindings (if (every #'(lambda (x) (and (consp x) (<= (length x) 3))) (second expr))
                          (second expr)
                          (error "Invalid binding in do expression: ~a" expr)))
            (new-bindings (mapcar (lambda (binding)
                                    (let* ((var (first binding))
                                           (init (second binding))
                                           (step (third binding)))
                                      (list (generate-unique-name var (incf counter))
                                            (rename-vars init env counter)
                                            (rename-vars step env counter))))
                                  bindings))
            (new-env (append (mapcar #'(lambda (x y) (cons (car x) (car y))) bindings new-bindings) env))
            (new-inits (mapcar (lambda (binding)
                                 (list (first binding)
                                       (second binding)
                                       (rename-vars (third binding) new-env counter))) new-bindings)))
       `(do ,new-inits
            ,(mapcar (lambda (e) (rename-vars e new-env counter)) (third expr))
          ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cdr (cddr expr))))))
    ;; If the expression is a LABELS or FLET forms, rename its function names, parameters, and bodies
    ((and (consp expr) (or (eq (first expr) 'labels) (eq (first expr) 'flet)))
     (let* ((bindings (if (every #'(lambda (x) (and (consp x) (symbolp (first x)) (consp (second x)))) (second expr))
                          (second expr)
                          (error "Invalid binding in expression: ~a" expr)))
            (new-bindings (mapcar (lambda (binding)
                                    (let* ((fn-name (first binding))
                                           (args (second binding))
                                           (new-args (mapcar (lambda (arg)
                                                               (generate-unique-name arg (incf counter)))
                                                             args))
                                           (fn-env (append (mapcar #'(lambda (x y) (cons x y)) args new-args) env)))
                                      `(,fn-name ,new-args ,@(mapcar (lambda (e) (rename-vars e fn-env counter)) (cddr binding)))))
                                  bindings)))
       `(labels ,new-bindings ,@(mapcar (lambda (e) (rename-vars e env counter)) (cddr expr)))))
    ;; If the expression is a do* form, rename its variable bindings and body
    ((and (consp expr) (eq (first expr) 'do*))
     (let* ((bindings (if (every #'(lambda (x) (and (consp x) (<= (length x) 3))) (second expr))
                          (second expr)
                          (error "Invalid binding in do* expression: ~a" expr)))
            (new-bindings nil)
            (new-env env))
       (dolist (binding bindings)
         (let* ((var (first binding))
                (init (second binding))
                (step (third binding))
                (new-var (generate-unique-name var (incf counter))))
           (push (list new-var
                       (rename-vars init new-env counter)
                       (rename-vars step new-env counter))
                 new-bindings)
           (push (cons var new-var) new-env)))
       `(do* ,(reverse new-bindings)
             ,(mapcar (lambda (e) (rename-vars e new-env counter)) (third expr))
          ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cdr (cddr expr))))))
    ;; If the expression is a MULTIPLE-VALUE-BIND, rename its variables and body
    ((and (consp expr) (eq (first expr) 'multiple-value-bind))
     (unless (and (listp (second expr)) ; Syntax checking
                  (every #'symbolp (second expr)))
       (error "Invalid bindings in MULTIPLE-VALUE-BIND expression: ~a" expr))
     (let* ((vars (second expr))
            (new-vars (mapcar (lambda (var)
                                (generate-unique-name var (incf counter)))
                              vars))
            (new-env (append (mapcar #'(lambda (x y) (cons x y)) vars new-vars) env)))
       `(multiple-value-bind ,new-vars
            ,(rename-vars (third expr) env counter)
          ,@(mapcar (lambda (e) (rename-vars e new-env counter)) (cdddr expr)))))
         
    ((consp expr)
     (cons (car expr) (mapcar (lambda (e) (rename-vars e env)) (cdr expr))))
    ;; Otherwise, return the expression as is
    (t expr)))


(defun chng-funcalls (fname c-id lexp)
  (cond ((null lexp) nil)
        ((and (listp lexp) (eq (car lexp) fname))
         (append (list (car lexp)) (chng-funcalls fname c-id (cdr lexp)) `((1+ ,c-id))))
        ((listp lexp)
         (append (list (chng-funcalls fname c-id (car lexp))) (chng-funcalls fname c-id (cdr lexp))))
        (t lexp)))


(defun get-decldoc (a &optional decls)
  (cond ((and (listp (car a)) (equal (caar a) 'declare))
         (get-decldoc (cdr a) (cons (car a) decls)))
        ((stringp (car a)) (list (append (reverse decls) (list (car a))) (cdr a)))
        (t (list (reverse decls) a))))

(defun chng-params (params c)
  (let ((o-pos (position '&optional params))
        (r-pos (position '&rest params))
        (k-pos (position '&key params)))
    (cond ((and (null o-pos) (null r-pos) (null k-pos)) 
           (append params `(&optional (,c 0))))
          ((and (null o-pos) r-pos)
           (append (subseq  params 0 r-pos) `(&optional (,c 0)) (subseq params r-pos))) 
          ((and (null o-pos) k-pos)
           (append (subseq  params 0 k-pos) `(&optional (,c 0)) (subseq params k-pos))) 
          ((and o-pos (null r-pos) (null k-pos))
           (append params (list `(,c 0))))
          ((and o-pos r-pos)
           (append (subseq params 0 r-pos) (list `(,c 0)) (subseq params r-pos)))
          ((and o-pos k-pos)
           (append (subseq params 0 k-pos) (list `(,c 0)) (subseq params k-pos))))))

(defmacro mod-fundef (fname params &body body)
  (let* ((tmp (get-decldoc body))
         (decls-doc (first tmp))
         (count (gensym "count"))
         (rest (chng-funcalls fname count (second tmp)))
         (newparams (chng-params params count)))
    `(defun ,fname ,newparams
       ,@decls-doc
       (if (> ,count *max-rec-depth*)
           (error "Function recursed for too long!...") 
           (progn
             ,@rest)))))

(defun mod-load-progfile (file)
  "Modifies the functions in the program so they to not cause
   stack overflow due to endless recursion."
  (let ((newfname (concatenate 'string (directory-namestring file) (string (gensym)))))
    (with-open-file (in file :direction :input)
      (with-open-file (out newfname :direction :output)
        (loop for form = (read in nil nil)
              while form
              do
                 (if (eq (car form) 'defun)
                     (progn
                       (pprint `(mod-fundef ,@(cdr form)) out)
                       (terpri out))
                     (progn (pprint form out)
                            (terpri out))))))
    (load newfname)
    (delete-file newfname)))

;; -----------


(defun rewrite-load (file)
  "Gets rid of CR characters in file creating new file, signals a warning, loads new file,
   and deletes it (if LOAD does not throw an error)"
  (let ((newfname (concatenate 'string (directory-namestring file) (string (gensym)))))
    (with-open-file (in file)
      (setf *cr-warning* newfname)
      (with-open-file (out newfname :direction :output)
        (do ((c (read-char in) (read-char in nil 'eof)))
            ((not (characterp c)))
          (if (char= c #\Return)
              (write-char #\Newline out)
              (write-char c out)))))
    (mod-load-progfile newfname)
                                        ;(load newfname)
    (delete-file newfname)))
  

(defun has-cr? (file)
  (with-open-file (in file)
    (do ((c (read-char in) (read-char in nil 'eof)))
        ((not (characterp c)))
      (when (char= c #\Return)
        (return t)))))

(defun load-solution (file)
  (if (has-cr? file)
      (rewrite-load file)
      (mod-load-progfile file)
                                        ;(load file)
      ))

(defun handle-solution-loading (student-solution)
  (handler-case (load-solution student-solution)
    (error (condition)
      (when (and *cr-warning* (probe-file *cr-warning*))
        (delete-file *cr-warning*))
      (push condition *load-error*))))
