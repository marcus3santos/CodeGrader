;;;; package.lisp

(defpackage #:sandbox
  (:documentation "Sandbox for code evaluation")
  (:use :cl))

(defpackage #:utils
  (:use #:cl)
  (:export #:read-form-and-intern #:with-package
           #:add-prefix-to-symbol-in-form))

(defpackage #:testing-runtime
  (:documentation "Creates the code testing runtime")
  (:use #:cl #:sandbox :fiveam :utils)
  (:export #:is-safe #:process-assessment-test-case-data))

(defpackage #:sxm-compiler
  (:nicknames #:sxm)
  (:use #:cl #:fiveam #:testing-runtime #:utils)
  (:export #:gen-exam-files))

(defpackage #:5ig
  (:use #:cl #:sxm-compiler #:testing-runtime)
  (:export #:gen-exam-files))
