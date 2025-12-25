;;;; ============================================================
;;;; Robust Tree Edit Distance for Lisp forms
;;;; Counts node insert/delete costs correctly and handles atoms
;;;; ============================================================

(defpackage :similarity-scorer
  (:use :cl :gensymifier)
  (:export :tree-edit-distance
           tree-size))

(in-package :similarity-scorer)

;;;; Helpers: treat any Lisp form as a tree node (atoms are leaves)
(defun leafp (x) (not (consp x)))

(defun node-label (x) (if (consp x) (car x) x))
(defun node-children (x) (if (consp x) (cdr x) nil))

;;;; Count nodes in a tree (each atom or cons counts as 1 node)
(defun tree-size (tree)
  (if (leafp tree)
      1
      (1+ (reduce #'+ (mapcar #'tree-size (node-children tree)) :initial-value 0))))

;;;; Cost functions (unit relabel cost; insert/delete cost = number of nodes inserted/removed)
(defun cost-relabel (a b)
  (if (equal (node-label a) (node-label b)) 0 1))

(defun cost-delete-subtree (tree) (tree-size tree))
(defun cost-insert-subtree (tree) (tree-size tree))

;;;; Sequence edit (Levenshtein-like) where:
;;;; - deleting element i costs cost-delete-subtree(child-i)
;;;; - inserting element j costs cost-insert-subtree(child-j)
;;;; - replacing i->j costs tree-edit-distance(child-i, child-j)
(defun seq-edit-distance (xs ys)
  (let* ((m (length xs))
         (n (length ys))
         (d (make-array (list (1+ m) (1+ n)) :initial-element 0)))
    ;; initialize first column/row
    (setf (aref d 0 0) 0)
    (loop for i from 1 to m do
          (setf (aref d i 0) (+ (aref d (1- i) 0) (cost-delete-subtree (nth (1- i) xs)))))
    (loop for j from 1 to n do
          (setf (aref d 0 j) (+ (aref d 0 (1- j)) (cost-insert-subtree (nth (1- j) ys)))))
    ;; fill table
    (loop for i from 1 to m do
          (loop for j from 1 to n do
            (let* ((del (+ (aref d (1- i) j) (cost-delete-subtree (nth (1- i) xs))))
                   (ins (+ (aref d i (1- j)) (cost-insert-subtree (nth (1- j) ys))))
                   (rep (+ (aref d (1- i) (1- j)) (tree-edit-distance (nth (1- i) xs) (nth (1- j) ys))))
                   (best (min del ins rep)))
              (setf (aref d i j) best))))
    (aref d m n)))

;;;; Main tree-edit-distance

(defun type-order (x)
  "Assign an ordering value to a type."
  (cond
    ((numberp x) 0)
    ((stringp x) 1)
    ((symbolp x) 2)
    ((listp x) 3)
    (t 4)))

(defun form< (a b)
  "Compare two Common Lisp forms A and B.
Returns T if A is considered less than B."
  (cond
    ;; Numbers: compare numerically
    ((and (numberp a) (numberp b))
     (< a b))

    ;; Strings: compare lexicographically
    ((and (stringp a) (stringp b))
     (string< a b))

    ;; Symbols: compare by name lexicographically
    ((and (symbolp a) (symbolp b))
     (string< (symbol-name a) (symbol-name b)))

    ;; Lists: compare element by element recursively
    ((and (listp a) (listp b))
     (cond
       ((null a) (not (null b)))   ; empty list is less than non-empty
       ((null b) nil)           ; non-empty list is greater than empty
       (t (or (form< (first a) (first b))
              (and (equal (first a) (first b))
                   (form< (rest a) (rest b)))))))

    ;; Different types: define some arbitrary type ordering
    (t
     (< (type-order a) (type-order b)))))

(defun sort-form (f)
  (cond ((null f) f)
        ((atom f) f)
        ((and (listp f)
              (or (eq (car f) '*)
                  (eq (car f) '+)))
         (cons (car f)
               (sort (mapcar #'sort-form (cdr f)) #'form<)))
        (t (cons (sort-form (car f)) (mapcar #'sort-form (cdr f))))))


(defun tree-edit-distance (t1 t2)
  (cond
    ;; both leaves (atoms or small lists): treat as one replacement
    ((and (leafp t1) (leafp t2))
     (if (equal t1 t2) 0 1))
    ;; leaf vs non-leaf = insert/delete whole subtree
    ((leafp t1)
     (cost-insert-subtree t2))
    ((leafp t2)
     (cost-delete-subtree t1))
    ;; otherwise combine relabel + child sequence edit
    (t
     (let ((label-cost (cost-relabel t1 t2)))
       (+ label-cost
          (seq-edit-distance (node-children t1)
                             (node-children t2)))))))
(defun normalize (f)
  (sort-form (macroexpand (normalize-gensyms (gensymify f)))))

(defun solution-distance (s1 s2)
  (let* ((ns1 (normalize s1))
         (ns2 (normalize s2)))
    (format t "Original:~%~s~%~s~%Normalized:~%~s~%~s~%" s1 s2 ns1 ns2)
    (tree-edit-distance ns1 ns2)))

(defun similarity (qs ss)
  (- 1 (float (/ (tree-edit-distance (normalize qs) (normalize ss)) (tree-size qs)))))


(defun get-call-graph (target-func program)
  "Returns a list of functions called by target-func within the given program."
  (let ((seen '())
        (graph '()))
    (labels ((find-defun (name)
               ;; Finds the body of a specific function in the program
               (cdr (assoc name 
                           (mapcan (lambda (form)
                                     (when (and (listp form) 
                                                (eq (first form) 'defun))
                                       (list (cons (second form) (cdddr form)))))
                                   program))))
             (extract-calls (body)
               ;; Recursively finds all symbols in the first position of a list
               (let ((calls '()))
                 (labels ((scan (item)
                            (cond ((null item) nil)
                                  ((listp item)
                                   (when (symbolp (car item))
                                     (pushnew (car item) calls))
                                   (mapc #'scan (reverse item))))))
                   (scan body)
                   calls)))
             (build-graph (func)
               ;; Traverses the calls to build the adjacency list
               (unless (member func seen)
                 (push func seen)
                 (let* ((body (find-defun func))
                        (calls (extract-calls body)))
                   (push (cons func (list calls)) graph)
                   (dolist (child calls)
                     (build-graph child))))))
      (build-graph target-func)
      graph)))


(defun score-similarity (target-func student-solution instructor-solutions)
  (let* ((student-solution-cg (get-call-graph target-func student-solution))
         (student-used-functions
           (remove nil (mapcar (lambda (func)
                                 (car (member func student-solution :key #'second)))
                               (mapcar #'first student-solution-cg)))))
    
    student-used-functions))
