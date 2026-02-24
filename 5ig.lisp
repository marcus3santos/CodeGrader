;;;; 5ig.lisp

(in-package #:5ig)

;;  Folder where the asssessment .data file should be stored

(defparameter *assessment-data-folder* "~/quicklisp/local-projects/5ig/Assessment-data/")

(defun derive-assessment-data-file (solution-file-path)
  "Derives the assessment data file path from the solution file path."
  (let* ((assessment-folder-name (car (last (pathname-directory solution-file-path))))
         (assessment-data-file (format nil "~a~a.data" *assessment-data-folder* assessment-folder-name)))
    assessment-data-file))

(defun probe-assessment-file (file)
  (let* ((pnd (pathname-directory file))
         (home (nth (- (length pnd) 2) pnd))
         (user-name (car (last (pathname-directory (user-homedir-pathname))))))
    (and (or (and (symbolp home)
                  (string= "HOME" (symbol-name home)))
             (and (stringp home)
                  (string= user-name home)))
         (probe-file file))))

(defun chk-my-solution (a#)
  "Checks a student's solution file against examples.
   a#: String identifying the solution file, e.g., '~/lab01/q1.lisp'."
  (unless (probe-assessment-file a#)
    (error "~%!!!!!!!! Error: You saved your file in the wrong folder. Please save it in the specified folder. !!!!!!!!"))
  (let* ((assessment-data-file (derive-assessment-data-file a#))
         (q-label (intern (string-upcase (pathname-name a#)) :keyword))
         (assessment-test-case-data (process-assessment-test-case-data assessment-data-file (list q-label)))
         (q-test-case-data (rest (assoc q-label assessment-test-case-data)))
         (fname (getf q-testcase-data :asked-function))
         (given-test-cases-metadata (getf q-test-case-data :given)))
    (with-package :testing-runtime
      ;; EVAL compiles/defines the test and runner in this package
      (eval given-test-cases-metadata)
      
      ;; 2. Perform the grading
      ;; grade-student uses (intern (format ...)) to find the runner
      ;; in the *package* where it is called.
      (grade-student a# q-label fname 'given)))
  t)

(defun test-grade-student (student-file assessment-data-file q-label)
  "Evaluates metadata in the :testing-runtime package and executes the grade."
  (let* ((assessment-test-case-data (process-assessment-test-case-data assessment-data-file (list q-label)))
         (q-testcase-data (rest (assoc q-label assessment-test-case-data)))
         (fname (getf q-testcase-data :asked-function))
         (given-testcases-metadata (getf q-testcase-data :given)))
    ;; 1. Set the evaluation context to :testing-runtime
    (with-package :testing-runtime
      ;; EVAL compiles/defines the test and runner in this package
      (eval given-testcases-metadata)
      
      ;; 2. Perform the grading
      ;; grade-student uses (intern (format ...)) to find the runner
      ;; in the *package* where it is called.
      (grade-student student-file q-label fname 'given))))


