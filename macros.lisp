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

(defun safely-load-std-solution (file)
  "Changes the current environment to the question's sandboxed environment  then loads
   the student's solution."
  (let ((current *package*))
    (in-package :sandbox)
    (load file)
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

(defun check-balanced-parentheses (code)
  "Returns T if CODE has balanced parentheses, otherwise NIL."
  (loop with depth = 0
        for char across code
        do (cond
             ((char= char #\() (incf depth))
             ((char= char #\)) (decf depth)
              (when (< depth 0) (return-from check-balanced-parentheses nil))))
        finally (return (= depth 0))))

(defun file-has-balanced-parentheses-p (filepath)
  "Reads Lisp source file and checks for balanced parentheses."
  (when (probe-file filepath)
    (with-open-file (in filepath :direction :input)
      (let ((content (make-string (file-length in))))
        (read-sequence content in)
        (check-balanced-parentheses content)))))

(defun has-cr? (file)
  (with-open-file (in file)
    (do ((c (read-char in) (read-char in nil 'eof)))
        ((not (characterp c)))
      (when (char= c #\Return)
        (return t)))))

(define-condition my-error (error)
  ((details :initarg :details :reader error-details))
  (:report (lambda (c s)
             (format s "My custom error: ~A" (error-details c)))))

(defun load-solution (file)
  (unless (file-has-balanced-parentheses-p file)
    (error 'my-error :details "Something specific broke"))
  (if (has-cr? file) (rewrite-load file)
      (safely-load-std-solution file)))

(defun handle-solution-loading (student-solution)
  (handler-case (load-solution student-solution)
    (my-error (e)
      (push e *load-error*))
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
