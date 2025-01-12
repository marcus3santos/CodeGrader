
(IN-PACKAGE :CL-USER)

(DEFPACKAGE #:SANDBOX
  (:USE CL)
  (:SHADOW "OPEN"
  #|          "LOAD"
            "EVAL"
            "DELETE-FILE"
            "WITH-OPEN-FILE"
            "RUN-PROGRAM"
            "SB-EXT:RUN-PROGRAM"
            "PROBE-FILE"
            "FILE-WRITE-DATE"
            "RENAME-FILE"
            "ENSURE-DIRECTORIES-EXIST"
            "DIRECTORY"
            "READ"
            "WRITE"
            "READ-LINE"
            "READ-FROM-STRING"
            "COMPILE"
            "COMPILE-FILE"
            "QUIT"
            "GC"
            "FORMAT"
            "DEFPACKAGE"
            "IN-PACKAGE"
    |#        
            )
  (:EXPORT F1 F2 F3))

(DEFPACKAGE #:RUNTIME
  (:USE :CL :SANDBOX)
  (:EXPORT *RESULTS*)
  (:EXPORT *RUNTIME-ERROR*)
  (:EXPORT *LOAD-ERROR*)
  (:EXPORT *CR-WARNING*)
  (:EXPORT *FORBIDDEN-SYMBOLS*)
  (:EXPORT *PENALTY-FORBIDDEN*)
  (:EXPORT :HANDLE-SOLUTION-LOADING))

(IN-PACKAGE :SANDBOX)

(DEFUN OPEN (&REST ARGS)
  (handler-case 
      (ERROR "!!!Access to function OPEN is restricted in this assessment!!!")
    (error (condition)
      (format t "~a~%" condition)
      (in-package :cl-user))))

#|
(DEFUN LOAD (&REST ARGS)
  (ERROR "Access to function LOAD is restricted in this assessment."))

(DEFUN EVAL (&REST ARGS)
  (ERROR "Access to function EVAL is restricted in this assessment."))

(DEFUN DELETE-FILE (&REST ARGS)
  (ERROR "Access to function DELETE-FILE is restricted in this assessment."))

(DEFUN WITH-OPEN-FILE (&REST ARGS)
  (ERROR "Access to function WITH-OPEN-FILE is restricted in this assessment."))

(DEFUN RUN-PROGRAM (&REST ARGS)
  (ERROR "Access to function RUN-PROGRAM is restricted in this assessment."))

(DEFUN |SB-EXT:RUN-PROGRAM| (&REST ARGS)
  (ERROR
   "Access to function SB-EXT:RUN-PROGRAM is restricted in this assessment."))

(DEFUN PROBE-FILE (&REST ARGS)
  (ERROR "Access to function PROBE-FILE is restricted in this assessment."))

(DEFUN FILE-WRITE-DATE (&REST ARGS)
  (ERROR
   "Access to function FILE-WRITE-DATE is restricted in this assessment."))

(DEFUN RENAME-FILE (&REST ARGS)
  (ERROR "Access to function RENAME-FILE is restricted in this assessment."))

(DEFUN ENSURE-DIRECTORIES-EXIST (&REST ARGS)
  (ERROR
   "Access to function ENSURE-DIRECTORIES-EXIST is restricted in this assessment."))

(DEFUN DIRECTORY (&REST ARGS)
  (ERROR "Access to function DIRECTORY is restricted in this assessment."))

(DEFUN READ (&REST ARGS)
  (ERROR "Access to function READ is restricted in this assessment."))

(DEFUN WRITE (&REST ARGS)
  (ERROR "Access to function WRITE is restricted in this assessment."))

(DEFUN READ-LINE (&REST ARGS)
  (ERROR "Access to function READ-LINE is restricted in this assessment."))

(DEFUN READ-FROM-STRING (&REST ARGS)
  (ERROR
   "Access to function READ-FROM-STRING is restricted in this assessment."))

(DEFUN COMPILE (&REST ARGS)
  (ERROR "Access to function COMPILE is restricted in this assessment."))

(DEFUN COMPILE-FILE (&REST ARGS)
  (ERROR "Access to function COMPILE-FILE is restricted in this assessment."))

(DEFUN QUIT (&REST ARGS)
  (ERROR "Access to function QUIT is restricted in this assessment."))

(DEFUN GC (&REST ARGS)
  (ERROR "Access to function GC is restricted in this assessment."))

(DEFUN FORMAT (&REST ARGS)
  (ERROR "Access to function FORMAT is restricted in this assessment."))

(DEFUN DEFPACKAGE (&REST ARGS)
  (ERROR "Access to function DEFPACKAGE is restricted in this assessment."))

(DEFUN IN-PACKAGE (&REST ARGS)
  (ERROR "Access to function IN-PACKAGE is restricted in this assessment."))
|#
