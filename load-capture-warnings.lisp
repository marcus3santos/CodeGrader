(defun load-and-capture-warnings (filepath)
  "Loads the Common Lisp file at FILEPATH and returns a string containing
   any warning, note, or error messages produced during the loading process
   that are directed to *error-output*.

   Args:
     FILEPATH: A string or pathname designating the Common Lisp file to load.

   Returns:
     A string containing the captured messages.
     Returns an empty string if no messages are captured.
  "
  (let ((messages (make-string-output-stream)))
    ;; Temporarily bind *error-output* to our string stream.
    ;; This ensures that any output from LOAD that goes to the error stream
    ;; is captured here instead of being printed to the console.
    (let ((*error-output* messages))
      (handler-case
          ;; Attempt to load the file.
          ;; The :verbose nil and :print nil arguments suppress
          ;; standard informational messages from LOAD itself,
          ;; focusing on actual warnings/errors.
          (load filepath :verbose nil :print nil)
        (error (c)
          ;; If an error occurs during loading (e.g., file not found,
          ;; syntax error that prevents loading), catch it and
          ;; print the error message to our stream.
          (format messages "~&Loading Error: ~A~%" c))))
    ;; Retrieve all collected messages from the string stream as a single string.
    (get-output-stream-string messages)))

;; --- Example Usage ---

;; To test this, you might create a dummy file, e.g., "student-code.lisp":
;;
;; ;;; student-code.lisp
;; (defun calculate-sum (a b)
;;   ;; This will cause a warning about an unused variable 'c'
;;   (let ((c 10))
;;     (+ a b)))
;;
;; (defun faulty-function ()
;;   ;; This will cause a compile-time error if 'non-existent-func' is not defined
;;   (non-existent-func 1 2))
;;
;; (format t "File loaded successfully!~%") ; This will go to *standard-output* and not be captured by this function

;; Example 1: Loading a file with warnings
;; (load-and-capture-warnings "path/to/your/student-code.lisp")

;; Example 2: Loading a non-existent file (will capture an error message)
;; (load-and-capture-warnings "non-existent-file.lisp")

;; Example 3: Loading a clean file (should return an empty string)
;; (load-and-capture-warnings "path/to/a/clean-file.lisp")
