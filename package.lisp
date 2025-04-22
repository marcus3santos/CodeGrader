(in-package :cl-user)

(defpackage #:sandbox
  (:documentation "Sandbox for code evaluation")
  (:use :cl :rutils)
  (:export :dangerous-function
           :dangerous-function-name)
  (:shadow open with-open-file load delete-file rename-file probe-file directory
           ensure-directories-exist file-author file-write-date file-length
           ql:quickload require))

(in-package :sandbox)

(define-condition dangerous-function (error)
  ((function-name :initarg :function-name :reader dangerous-function-name))
  (:report (lambda (condition stream)
             (format stream "Use of forbidden function: ~A"
                     (function-name condition)))))

(defmacro define-dangerous-function (name)
  `(defun ,name (&rest args)
     (declare (ignore args))
     (error 'dangerous-function :function-name ',name)))
#|
(dolist (fn '(open with-open-file load delete-file rename-file probe-file directory
              ensure-directories-exist file-author file-write-date file-length
              #|
              run-program ext:run-shell-command sb-ext:run-program uiop:run-program
              sb-ext:quit ext:exit sb-ext:exit uiop:quit quit sleep

              open-stream-p make-socket usocket:socket-connect usocket:socket-listen
              usocket:socket-accept usocket:socket-close
              |#
              intern eval compile compile-file load
              #|
              inspect disassemble describe room gc sb-ext:gc
              trace untrace sb-int:with-fasl-lock
              |#
              defmacro defmethod define-method-combination defpackage defclass
              in-package

              catch throw restart-case restart-bind handler-bind handler-case
              ignore-errors))
  (eval `(define-dangerous-function ,fn)))

|#

(dolist (fn '(open with-open-file load delete-file rename-file probe-file directory
              ensure-directories-exist file-author file-write-date file-length
              ql:quickload require))
  (eval `(define-dangerous-function ,fn)))

(in-package :cl-user)

(defpackage #:test-runtime
  (:documentation "Creates the code testing runtime")
  (:use cl :sandbox)
  (:export *question*)
  (:export *results*)
  (:export *runtime-error*)
  (:export *load-error*)
  (:export *cr-warning*)
  (:export *forbidden-symbols*)
  (:export *penalty-forbidden*)
  (:export *assessment-data-folder*)
  (:export :handle-solution-loading :load-macros :load-test-cases))

(defpackage #:grader
  (:documentation "Creates the code grading apparatus")
  (:use cl :test-runtime)
  (:export :evaluate-solution)
  (:export :grade-code))

(defpackage :sexprmark-to-org
  (:nicknames :sxm)
  (:use :cl)
  (:export :str->list))

(defpackage #:codegrader
  (:documentation "Manages the grading of all assignment submissions")
  (:use cl :grader :test-runtime :sexprmark-to-org)
  (:export :grade-it :grade-exam :eval-solutions :eval-student-solutions :chk-my-solution))

(defpackage #:cg
  (:documentation "CL-USER + Codegrader utilities")
  (:use :common-lisp :cl-user :codegrader :grader)
  (:export :start :quit :eval-solutions :grade-it  :grade-exam :evaluate-solution :eval-student-solutions :chk-my-solution))


