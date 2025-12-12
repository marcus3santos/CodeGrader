;;;; ============================================================
;;;; Robust Tree Edit Distance for Lisp forms
;;;; Counts node insert/delete costs correctly and handles atoms
;;;; ============================================================

(defpackage :tree-edit
  (:use :cl :normalize)
  (:export :tree-edit-distance
           tree-size))

(in-package :tree-edit)

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


(defun solution-distance (s1 s2)
  (tree-edit-distance (normalize s1) (normalize s2)))

(defun similarity (qs ss)
  (- 1 (float (/ (tree-edit-distance (normalize qs) (normalize ss)) (tree-size qs)))))

(defun test-cases ()
  (mapcar (lambda (a)
            (let ((res (eval a)))
              (format t "~:[Failed~;Passed~] ~a~%" res a)))
          '((= (solution-distance
                '(defun test () 1)
                '(defun test () 2))
             1)
            (= (solution-distance
                '(defun test (x) x)
                '(defun test (y) x))
             1)
            (= (solution-distance
                '(defun test (x) x)
                '(defun test1 (y) y))
             1)
            (= (solution-distance
                '(defun test (x)
                   (dotimes (i p r)
                     (setf i 0))
                   (dotimes (i p r)
                     (setf i 0)))
                '(defun test (x)
                   (dotimes (i p r)
                     (setf i 0))
                   (dotimes (k p r)
                     (setf k 0))))
             0)
            (= (solution-distance
                '(defun ht-get (key ht)
                  (let ((size (length (ht-array ht)))  
                        (start (rem (sxhash key) size)))   ; sxhash is a CL function
                   (do* ((count 0 (1+ count))
                         (i start (rem (1+ i) size))
                         (item (aref (ht-array ht) start) 
		               (aref (ht-array ht) i))) 
                        ((or (null item)
                             (= count size)))
                     (when (and (funcall (ht-comp ht) key (car item))
		                (not (eql (cdr item) 'deleted))) ; checks it's not a deleted item
                       (return 
                         (values (cdr item)
                                 ;; the second value is an index, at which the item was found
                                 ;; (also used to distinguish the value nil from not found,
                                 ;; which is also represented by nil but with no second value)
                                 i))))))
                '(defun ht-get (key ht)
                  (let* ((size (length (ht-array ht)))  
                             (start (rem (sxhash key) size)))   ; sxhash is a CL function
                   (do* ((count1 0 (1+ count1))
                         (i start (rem (1+ i) size))
                         (item1 (aref (ht-array ht) start) 
		               (aref (ht-array ht) i))) 
                        ((or (null item1)
                             (= count1 size)))
                     (if (and (funcall (ht-comp ht) key (car item1))
		                (not (eql (cdr item) 'deleted))) ; checks it's not a deleted item
                       (return 
                         (values (cdr item1)
                                 ;; the second value is an index, at which the item was found
                                 ;; (also used to distinguish the value nil from not found,
                                 ;; which is also represented by nil but with no second value)
                                 i)))))))
            3))))
