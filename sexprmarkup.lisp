(defpackage :sexprmark-to-org
  (:use :cl)
  (:export :sexprmark->org))

(in-package :sexprmark-to-org)

;; Utility functions

(defun indent (n)
  (make-string (* 2 n) :initial-element #\Space))

(defun stars (n)
  (make-string n :initial-element #\*))

(defun flatten (lst)
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    ((append (flatten (car lst)) (flatten (cdr lst))))))

(defun trim-spc-last (strings)
  (let ((trimmed (append (butlast strings)
                         (list (string-right-trim " " (car (last strings)))))))
    trimmed))

;; Serializer


(defun sexprmark->org (sexpr)
  (labels
      ((emit (node &optional (depth 0))
         (cond
           ((consp node)
            (case (car node)
              (doc
               (let ((title (getf (cdr node) :title))
                     (children (cdddr node)))
                 (cons (format nil "#+TITLE: ~a~%" title)
                       (mapcar #'emit children))))             
              (section
               (let ((level (getf (cdr node) :level))
                     (title (getf (cdr node) :title))
                     (children (cdr (cddddr node))))
                 (cons (format nil "~a ~a~%" (stars level) title)
                       (mapcar #'emit children))))
              (p
               (append (mapcar #'emit (cdr node))
                       (list (format nil "~%"))))
              (ul
               (mapcar (lambda (item)
                         (cons (format nil "~%") (emit item (1+ depth))))
                       (cdr node)))
              (li
               (let ((item (cdr node)))
                 (cond
                   ((and (consp (car item)) (eq (caar item) 'todo))
                    (format nil "~a- [ ] ~{~a~}" (indent depth) (flatten (mapcar (lambda (e)
                                                                                   (emit e depth))
                                                                                 (cdar item)))))
                   ((consp item)
                    (format nil "~a- ~{~a~}" (indent depth) (flatten (mapcar (lambda (e)
                                                                               (emit e depth))
                                                                             item))))
                   (t (error "Improper item ~a" item)))))
              (hl (let ((link (second node))
                        (text (third node)))
                    (list (format nil (if text  "[[~a][~a]] " "[[~a]]") link text))))
              ($
               (list (format nil "$~{~a~}$ " (cdr node))))
              (b
               (list (format nil "*~{~a~}* " (trim-spc-last (mapcar #'emit (cdr node))))))
              (tt
               (list (format nil "=~{~a~}= " (trim-spc-last (mapcar #'emit (cdr node))))))
              (em
               (list (format nil "/~{~a~}/ " (trim-spc-last (mapcar #'emit  (cdr node))))))
              (rp
               (list (format nil "(~{~a~}) " (trim-spc-last (mapcar #'emit  (cdr node))))))
              
              
              (code-block
               (let ((lang (getf (cdr node) :lang))
                     (lines (cdddr node)))
                 (cons (format nil "~%#+BEGIN_SRC ~a~%" lang)
                       (append (mapcar (lambda (line) (format nil "~a~%" line)) lines)
                               (list (format nil "#+END_SRC~%"))))))
              (cc (if (and (symbolp (cadr node))
                           (null (cddr node)))
                      (let ((symbname (symbol-name (cadr node))))
                        (format nil "~a " (concatenate 'string 
                                                       (string (aref symbname 0)) 
                                                       (string-downcase (subseq symbname 1)))))))
              (t (format nil "Invalid node: ~a" node))))
           ((symbolp node) (format nil "~a " (string-downcase (symbol-name node))))
           ((atom node) (format nil "~a " node)))))
    (format nil "~{~a~}" (flatten  (emit sexpr)))))

(defun test ()
  (let ((sexpr
          '(doc :title "PT 1"
            (section :level 1 :title "Heading 1"
             (p "Some paragraph text under \"a\" heading 1.")
             (p "This" is a text.))
            (section :level 2 :title "Heading 2"
             (p "Another" paragraph  ($ "x=2")  and (em the rest.))
             (P "Write" a (tt function)  (b count-occurrences and another) )
             (ul
              (li (cc Item) one)
              (li (cc Item) two
               (ul
                (li (cc Subitem)
                 (ul
                  (li (cc Subsubitem))))
                (li (cc Another) item)))
              (li (todo (cc Task))))
             (p)
             (code-block :lang "python"
              "print(\"Hello, Org!\")"))
            )))
    (sexprmark->org sexpr)))
