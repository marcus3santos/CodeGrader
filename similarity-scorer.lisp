;;;; ============================================================
;;;; Robust Tree Edit Distance for Lisp forms
;;;; Counts node insert/delete costs correctly and handles atoms
;;;; ============================================================

(in-package :similarity-scorer)

;;;; Helpers: treat any Lisp form as a tree node (atoms are leaves)
(defun leafp (x) (not (consp x)))

(defun node-label (x) (if (consp x) (car x) x))
(defun node-children (x) (if (consp x) (cdr x) nil))

;;;; Count nodes in a tree (each atom or cons counts as 1 node)

(defun tree-size (tree)
  (cond ((null tree) 0)
        ((leafp tree) 1)
        ((listp (first tree))
         (+ (tree-size (first tree))
            (tree-size (rest tree))))
        (t (+ 1 (tree-size (rest tree))))))

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
              (eq (first f) 'labels))
         `(,(first f)
           ,(sort (mapcar #'sort-form (second f)) #'form<)
           ,@(sort-form (cddr f))))
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
    ((and (listp t1) (listp t2))
     (+ (tree-edit-distance (first t1) (first t2))
        (seq-edit-distance (node-children t1)
                           (node-children t2))))
    ;; otherwise combine relabel + child sequence edit
    (t
     (let ((label-cost (cost-relabel t1 t2)))
       (+ label-cost
          (seq-edit-distance (node-children t1)
                             (node-children t2)))))))


(defun normalize (f)
  (sort-form (normalize-gensyms (gensymify f))))

(defun normalize-expand (f)
  (let* ((gensymified (gensymify f))
         (normed-gensyms (normalize-gensyms gensymified))
         (macroexpanded (macroexpand normed-gensyms)))
    (sort-form macroexpanded)))

#|
(defun similarity (qs ss &optional (lambda 0.1))
  (let* ((nqs (normalize-expand qs))
         (nss (normalize-expand ss))
         (distance (tree-edit-distance nqs nss))
         (size (tree-size nqs))
         ;; We set lambda so that a distance equal to the 
         ;; tree-size results in a ~36% score (e^-1).
         (dynamic-lambda (/ 1.0 size)))
    ;; Formula: e^(-lambda * distance)
    (exp (* (- dynamic-lambda) distance))))

(defun similarity (qs ss) 
  (let* ((nqs (normalize-expand qs)) 
         (nss (normalize-expand ss)) 
         (distance (tree-edit-distance nqs nss)) 
         ;; Measure both trees to find the maximum possible "cost"
         (size-qs (tree-size nqs))
         (size-ss (tree-size nss))
         (max-size (max size-qs size-ss)))
    (if (zerop max-size)
        1.0 ;; Two empty forms are 100% similar
        (max 0.0 (- 1 (/ (float distance) max-size))))))


(defun similarity (qs ss) 
  (let* ((nqs (normalize-expand qs)) 
         (nss (normalize-expand ss)) 
         (distance (tree-edit-distance nqs nss)) 
         ;; Measure both trees to find the maximum possible "cost"
         (size-qs (tree-size nqs))
         (size-ss (tree-size nss))
         (avg-size (/ (+ size-qs size-ss) 2.0)))
    (if (zerop avg-size)
        1.0 ;; Two empty forms are 100% similar
        (max 0.0 (- 1 (/ distance avg-size))))))
|#

(defun similarity (qs ss)
  "Calculates a similarity score between 0.0 and 1.0 for two Lisp forms.
   
   This function uses a normalized Tree Edit Distance approach based on the 
   Sørensen-Dice coefficient. It is specifically designed to be more lenient 
   than a simple linear ratio of distance to instructor-tree-size.

   ALGORITHM:
   1. Normalizes and expands both forms (qs = instructor, ss = student).
   2. Calculates the tree edit distance (D).
   3. Computes the total complexity as the sum of the sizes of both trees (S1 + S2).
   4. Applies the formula: 1.0 - (2 * D / (S1 + S2)).
   5. Clips the result at 0.0 to prevent negative scores.

   LENIENCY LOGIC:
   Unlike (1 - D/S1), which hits zero as soon as the distance equals the 
   instructor's code size, this version allows for 'structural drift.' 
   A student can write significantly more code (increasing S2) without 
   bottoming out the score, provided the distance doesn't grow faster 
   than the total complexity.

   EDGE CASES:
   - If both forms are empty, returns 1.0.
   - If one form is empty (e.g., NIL) and the other is a substantial 
     function, the score will approach 0.0.
   - Returns a FLOAT for precision."
  (let* ((nqs (normalize-expand qs))
         (nss (normalize-expand ss))
         (distance (tree-edit-distance nqs nss))
         (s1 (tree-size nqs))
         (s2 (tree-size nss))
         (total (+ s1 s2)))
    (cond 
      ((zerop total) 1.0)
      (t (max 0.0 (float (- 1 (/ (* 2.0 distance) total))))))))

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
                                   (if (eq (first item) 'function) ; handles #'  symbols
                                       (pushnew (cadr item) calls)
                                       (when (car item)
                                         (pushnew  (car item) calls)))
                                   (mapc #'scan (reverse  item))))))
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

(defun get-relevant-code (program call-graph)
  (let ((form-map (make-hash-table :test 'eq))
        (filtered-program
          (remove-if-not (lambda (f)
                           (and (consp f)
                                (member (car f) '(let let* defun defconstant defparameter defvar))))
                    program)))
    ;; Index the program once: O(N)
    (dolist (form filtered-program)
      (setf (gethash (second form) form-map) form))
    
    (append
     ;; Filter constants: O(N)
     (remove-if-not (lambda (f)
                      (and (consp f)
                           (member (car f) '(defconstant defparameter defvar))))
                    filtered-program)
     ;; Map call-graph nodes to forms: O(M)
     (loop for (node-name has-target) in call-graph
           when (and has-target (gethash node-name form-map))
             collect (gethash node-name form-map)))))

(defun embed-helpers (main-func-name program)
  (let* ((globals (loop for form in program
                        when (and  (consp form) (member (car form) '(defconstant defvar 'defparameter)))
                          collect form))
         (helpers (loop for form in program
                        when (and (consp form)
                                  (eq (first form) 'defun)
                                  (not (eq (second form) main-func-name)))
                          collect form))
         (main-def (first (member main-func-name program :key #'second)))
         (main-lamblist (third main-def))
         (main-bdy (cdddr main-def)))
    (if (and main-def helpers)
        (append  globals
                 (list`(defun ,main-func-name ,main-lamblist
                         (labels ,(mapcar #'rest helpers)
                           ,@main-bdy))))
        program)))

(defun filter-atoms (program)
  (cond ((null program) program)
        ((atom (car program)) (filter-atoms (cdr program)))
        (t (cons (car program) (filter-atoms (cdr program))))))

(defun closest-solution-to-the-profs-solution (std-sol prof-sols)
  (let ((dist most-positive-fixnum)
        res)
    (mapc (lambda (s)
            (let ((temp (tree-edit-distance (normalize-expand std-sol)
                                            (normalize-expand s))))
              (when (< temp dist)
                (setf dist temp)
                (setf res (normalize s)))))
          prof-sols)
    res))

(defun score-similarity (target-func raw-student-solution instructor-solutions)
  "Return the similarity score (0 to 1.0) between the definition for function
   TARGET-FUNC present in the STUDENT-SOLUTION list and the one found in the 
   INSTRUCTOR-SOLUTIONS list. The latter is in the form 
   (doc (defun ...) ...)"
  (let* ((student-solution (filter-atoms raw-student-solution))
         (student-solution-cg (get-call-graph target-func student-solution))
         (student-used-functions-and-globals
           (get-relevant-code student-solution student-solution-cg))
         (student-sol-with-embedded-helpers (embed-helpers target-func student-used-functions-and-globals))
         (instructor-solutions-for-target-func
           (loop for s in instructor-solutions
                 for data = (rest s)
                 when (second (assoc target-func (get-call-graph target-func data)))
                   collect data))
         (instructor-solutions-used-functions-and-globals
           (mapcar (lambda (instructor-solution)
                     (get-relevant-code instructor-solution (get-call-graph target-func instructor-solution)))
                   instructor-solutions-for-target-func))
         (instructor-sols-with-embedded-helpers
           (mapcar (lambda (sol)
                     (embed-helpers target-func sol))
                   instructor-solutions-used-functions-and-globals))
         (max-similarity))
    (mapc (lambda (instructor-solution)
            (let ((similarity-score (similarity instructor-solution student-sol-with-embedded-helpers)))
              (when (or (null max-similarity)
                        (> similarity-score (first max-similarity)))
                (setf max-similarity (list similarity-score (normalize instructor-solution) (normalize student-sol-with-embedded-helpers))))))
          instructor-sols-with-embedded-helpers)
    (if (zerop (first max-similarity))
        (list (first max-similarity)
              (closest-solution-to-the-profs-solution student-sol-with-embedded-helpers instructor-sols-with-embedded-helpers)
              (third max-similarity))
        max-similarity)))
