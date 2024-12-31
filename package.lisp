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

(defpackage #:cg-sandbox
  (:use :cl :rutils)
  (:shadow 
   "OPEN" "LOAD" "EVAL" "DELETE-FILE" 
   "WITH-OPEN-FILE" "RUN-PROGRAM" "SB-EXT:RUN-PROGRAM"
   "PROBE-FILE" "FILE-WRITE-DATE" "RENAME-FILE" "ENSURE-DIRECTORIES-EXIST"
   "DIRECTORY" "READ" "WRITE" "READ-LINE" "READ-FROM-STRING"
   "COMPILE" "COMPILE-FILE" "QUIT" "GC" "FORMAT"
   "DEFPACKAGE" "IN-PACKAGE"))

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

(defun with-open-file (&rest args)
  (error "Access to WITH-OPEN-FILE is restricted in the sandbox."))

(defun run-program (&rest args)
  (error "Access to RUN-PROGRAM is restricted in the sandbox."))

(defun probe-file (path)
  (error "Access to PROBE-FILE is restricted in the sandbox."))

(defun file-write-date (path)
  (error "Access to FILE-WRITE-DATE is restricted in the sandbox."))

(defun rename-file (old-path new-path)
  (error "Access to RENAME-FILE is restricted in the sandbox."))

(defun ensure-directories-exist (path)
  (error "Access to ENSURE-DIRECTORIES-EXIST is restricted in the sandbox."))

(defun directory (&optional pathname)
  (error "Access to DIRECTORY is restricted in the sandbox."))

(defun read (&rest args)
  (error "Access to READ is restricted in the sandbox."))

(defun write (&rest args)
  (error "Access to WRITE is restricted in the sandbox."))

(defun read-line (&rest args)
  (error "Access to READ-LINE is restricted in the sandbox."))

(defun read-from-string (string &optional start end &key (eof-error-p t) eof-value)
  (error "Access to READ-FROM-STRING is restricted in the sandbox."))

(defun compile (function &optional name)
  (error "Access to COMPILE is restricted in the sandbox."))

(defun compile-file (pathname &key &allow-other-keys)
  (error "Access to COMPILE-FILE is restricted in the sandbox."))

(defun quit (&rest args)
  (error "Access to SB-EXT:QUIT is restricted in the sandbox."))

(defun gc ()
  (error "Access to GC is restricted in the sandbox."))

(defun format (&rest args)
  (error "Access to FORMAT is restricted in the sandbox."))

(defun defpackage (&rest args)
  (error "Access to DEFPACKAGE is restricted in the sandbox."))

(defun in-package (package)
  (error "Access to IN-PACKAGE is restricted in the sandbox."))
