(in-package :cl-user)

(defpackage #:sandbox
  (:documentation "Sandbox for code evaluation")
  (:use :cl :rutils)
  (:export :dangerous-function
           :dangerous-function-name)
  (:shadow :open))

(in-package :sandbox)

(define-condition dangerous-function (error)
  ((function-name :initarg :function-name :reader dangerous-function-name))
  (:report (lambda (condition stream)
             (format stream "Use of forbidden function: ~A"
                     (function-name condition)))))

(defun open (&rest args)
  (declare (ignore args))
  (error 'dangerous-function :function-name 'open))

(in-package :cl-user)

(defpackage #:test-runtime
  (:documentation "Creates the code testing runtime")
  (:use cl :sandbox)
  (:export *results*)
  (:export *runtime-error*)
  (:export *load-error*)
  (:export *cr-warning*)
  (:export *forbidden-symbols*)
  (:export *penalty-forbidden*)
  (:export :handle-solution-loading))

(defpackage #:grader
  (:documentation "Creates the code grading apparatus")
  (:use cl :test-runtime)
  (:export :evaluate-solution)
  (:export :grade-code))


(defpackage #:codegrader
  (:documentation "Manages the grading of all assignment submissions")
  (:use cl :grader :test-runtime)
  (:export :grade-it :grade-exam :eval-solutions :eval-student-solutions :ck-my-solution))

(defpackage #:cg
  (:documentation "CL-USER + Codegrader utilities")
  (:use :common-lisp :cl-user :codegrader :grader)
  (:export :start :quit :eval-solutions :grade-it  :grade-exam :evaluate-solution :eval-student-solutions :ck-my-solution))


