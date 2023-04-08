(in-package :asdf-user)

(defsystem #:codegrader
  :name "CodeGrader"
  :description "Codegrader is an automated grader for lisp program assignments where the students' assignment solutions have been exported from D2L."
  :author "Marcus Santos <m3santos@ryerson.ca>"
  :license  "Specify license here"
  :version "0.2"
  :serial t
  :depends-on (#:zip :rutils)
  :components ((:file "package")
               (:file "macros")
               (:file "grader")
               (:file "codemeter")
               (:file "codegrader")))
