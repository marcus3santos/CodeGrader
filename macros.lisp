;; macros.lisp

(in-package :test-runtime)

(defparameter *system-name* :codegrader)

(defparameter *assessment-data-folder* "~/Codegrader/")

;; Unit test macros

(defparameter *question* nil)

(defparameter *results* nil)

(defvar *runtime-error* nil)

(defvar *load-error* nil)

(defvar *cr-warning* nil)

(defparameter *forbidden-symbols* nil) ;; List containing the names of the symbols students are not allowed to use

(defparameter *penalty-forbidden* 0.5) ;; Penalty (to be multiplied by total lab mark) for using forbidden symbols

(defparameter *test-name* nil)

; Defines the max depth of recursion for functions

(defparameter *max-depth* 20000)



;; Maximum running time (in seconds) allotted to the
;; evaluation of a test case. Once that time expires, the respective
;; thread is terminated, and a fail is associated to the respective
;; test case 

(defvar *max-time* 0.1) 


(defun report-result (result form)
  (let ((res (not (or (typep result 'condition)
                      (typep result 'sb-ext:timeout)
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


(define-condition computation-too-long (error)
  ((timeout :initarg :timeout :reader computation-too-long-timeout))
  (:report (lambda (condition stream)
             (format stream "Execution timed out after ~a seconds. The program is taking too long to complete, possibly due to infinite recursion or an endless loop. Please check your logic and consider adding a termination condition.~%" (computation-too-long-timeout condition)))))


(defmacro time-execution (expression max-time)
  "Evaluates the given expression and returns its result. If the expression
   does not complete within the specified max-time (in seconds), it returns
   a timeout error condition. If the expression triggers a stack overflow
   the store-condition captures that condition. Other kinds of errors are
   captured by the general ERROR condition"
  (let ((c (gensym)))
    `(handler-case
         (sb-ext:with-timeout ,max-time
           (handler-case ,expression
             (storage-condition (,c)
               ,c)
             (error (,c)
               ,c)))
       (sb-ext:timeout (,c)
         ,c))))

(defmacro check (&body forms)
  `(combine-results                     
     ,@(loop for f in forms collect     
             `(report-result (time-execution ,f ,*max-time*) ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (setf *test-name* ',name)
     ,@body))

(defun whats-asked (question)
  (setf *question* question))

(defun forbidden-symbols (&key (penalty .5) symbols)
  (progn 
    (setf *penalty-forbidden* penalty)
    (setf *forbidden-symbols* symbols)))

(defun replace-in-expr (s ns expr)
  (cond ((null expr) expr)
        ((eq expr s) ns)
        ((listp expr)
         (cons (replace-in-expr s ns (car expr))  
               (mapcar (lambda (item) (replace-in-expr s ns item)) (cdr expr)))) 
        (t expr)))

(defun params2args (params &optional oflag kflag acc)
  (cond ((null params) (reverse acc))
        ((eq (car params) '&rest) (reverse (cons (cdr params) acc)))
        ((eq (car params) '&optional)
         (when kflag (error "&key should not come before &optional"))
         (params2args (cdr params) t kflag acc))
        ((eq (car params) '&key) (params2args (cdr params) oflag t acc))
        ((and kflag (listp (first params)))
         (params2args (cdr params) oflag kflag
                      (append (list (second (first params))
                                    (intern (symbol-name (first (first params))) :keyword)) acc)))
        ((and oflag (listp (first params)))
         (params2args (cdr params) oflag kflag (cons (second (first params)) acc)))
        ((and oflag (null (first params)))
         (params2args (cdr params) oflag kflag (cons nil acc)))
        (t (params2args (cdr params) oflag kflag (cons (first params) acc)))))


(defun get-fname (s)
  (let ((name (symbol-name s)))
    (string-upcase (subseq name 0 (position #\Space name :test #'char-equal)))))

(defmacro wrp-defun (defun)
  (unless (listp (third defun))
    (error "Invalid syntax for DEFUN form!"))
  (let* ((name (second defun))
         (params (third defun))
         (bdy (cdddr defun))
         (new-name (gensym (format nil "~a " (symbol-name name))))
         (depth (gensym "DEPTH-"))
         (max-depth (gensym "*max-depth*"))
         (new-params (replace-in-expr name new-name params))
         (args (params2args new-params))
         (new-bdy (replace-in-expr name new-name bdy)))
    `(defun ,name (,@new-params)
       (let ((,depth 0)
             (,max-depth ,*max-depth*))
         (labels ((,new-name (,@new-params)
                    (cond ((< ,depth ,max-depth)                     
                           (incf ,depth)
                           ,@(if (= (length new-bdy) 1)
                                 `((car (list ,@new-bdy)))
                                 new-bdy))
                          ((error ,(format nil "Recursion too deep in function ~a !" (get-fname new-name)))))))
           (apply #',new-name (list ,@args)))))))

(defun wrp-load-std-sols (file)
  "This function should be used for loading the student's lisp file.
   It reads the forms in the student's lisp file.  If a form is a DEFUN, uses a macro 
   to rewrite the function to avoid it causing a stack overflow when the function is called, 
   then evals the form. Otherwise, evals the form."
  (with-open-file (in file :direction :input)
    (loop for form = (read in nil nil)
          while form 
          do (eval (if (and (consp form) (eq (car form) 'defun))
                       `(wrp-defun ,form)
                       form)))))

(defun safely-load-std-solution (file)
  "Changes the current environment to the question's sandboxed environment  then loads
   the student's solution."
  (let ((current *package*))
    (in-package :sandbox)
    (load file)
    ;;(wrp-load-std-sols file)
    (setf *package* current)))

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
    (safely-load-std-solution newfname)
    ;;(load newfname)
    (delete-file newfname)))
  

(defun has-cr? (file)
  (with-open-file (in file)
    (do ((c (read-char in) (read-char in nil 'eof)))
        ((not (characterp c)))
      (when (char= c #\Return)
        (return t)))))

(defun load-solution (file)
  (if (has-cr? file) (rewrite-load file)
      (safely-load-std-solution file)))

(defun handle-solution-loading (student-solution)
  (handler-case (load-solution student-solution)
    (error (condition)
      (when (and *cr-warning* (probe-file *cr-warning*))
        (delete-file *cr-warning*))
      (push condition *load-error*))))

(defun load-macros ()
  (load (merge-pathnames (asdf:system-source-directory *system-name*) "macros.lisp")))

(defun load-test-cases (kind assessmt-question)
  (let* ((assessment (car assessmt-question))
         (question (cadr assessmt-question))
         (assessment-data-file (format nil "~a~a.data" *assessment-data-folder* assessment))
         (assessment-data
           (handler-case (with-open-file (in assessment-data-file :direction :input)
                           (read in))
             (file-error (e) "Assessment data file error: ~a" e)))
         (question-data (cdr (assoc question assessment-data :test #'string=)))
         (testcase-code (cdr (assoc kind question-data :test #'string=))))
    (dolist (code testcase-code)
      (eval code))))
