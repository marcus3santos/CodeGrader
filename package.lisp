(in-package :cl-user)

(defpackage #:test-runtime
  (:documentation "Creates the code testing runtime")
  (:use cl :rutils)
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

#|
(defpackage #:codemeter
  (:documentation "Measures program size")
  (:use cl :test-runtime)
  (:export :program-size))
|#

(defpackage #:codegrader
  (:documentation "Manages the grading of all assignment submissions")
  (:use cl :grader :test-runtime)
  (:export :grade-it :grade-exam :eval-solutions :eval-student-solutions :ck-my-solution))

(defpackage #:cg
  (:documentation "CL-USER + Codegrader utilities")
  (:use :common-lisp :cl-user :codegrader :grader)
  (:export :start :quit :eval-solutions :grade-it  :grade-exam :evaluate-solution :eval-student-solutions :ck-my-solution))




