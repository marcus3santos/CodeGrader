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


(defpackage #:codegrader
  (:documentation "Manages the grading of all assignment submissions")
  (:use cl :grader :test-runtime)
  (:export :grade-it :grade-exam :eval-solutions :eval-student-solutions :ck-my-solution))

(defpackage #:cg
  (:documentation "CL-USER + Codegrader utilities")
  (:use :common-lisp :cl-user :codegrader :grader)
  (:export :start :quit :eval-solutions :grade-it  :grade-exam :evaluate-solution :eval-student-solutions :ck-my-solution))

#|
(defpackage :q_i
  (:documentation "Dedicated package for the student's solution store in file q_i,lisp, so it does not polute CodeGrader's name space")
  (:use cl)
  (:export <All functions asked in a question>))

The idea here is to have a qpackages.lisp file containing one defpackage for each question of an assessment.

The folder containing the defpackages for an assessment will be provided as a new argument to cg:grade-exam ;

Codegrader will load all defpackages before loading and running the student's solutions.

Then, in the test cases file for a given question, q_i, where a given function func is tested, we will refer to that function by mentioning its package q_i:func
|#

(defpackage #:cg-sandbox
  (:use :cl)
  (:shadow "OPEN" "LOAD" "EVAL" "DELETE-FILE")) ; Shadow restricted functions

(in-package :cg-sandbox)

;; Redefine restricted functions
(defun open (path &key (direction :input) &allow-other-keys)
  (error "Access to OPEN is restricted in the sandbox."))

(defun load (path &key &allow-other-keys)
  (error "Access to LOAD is restricted in the sandbox."))

(defun eval (form)
  (error "Access to EVAL is restricted in the sandbox."))

(defun delete-file (path)
  (error "Access to DELETE-FILE is restricted in the sandbox."))
