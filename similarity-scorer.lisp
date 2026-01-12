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
  (let* ((gensymified (gensymify f))
         (normed-gensyms (normalize-gensyms gensymified)))
    (sort-form (macroexpand normed-gensyms))))


(defun similarity (qs ss)
  (let* ((nqs (normalize qs))
         (nss (normalize ss))
         (distance (tree-edit-distance nqs nss))
        (tree-size (tree-size qs)))
    (if (> distance tree-size)
        0
        (- 1 (float (/ distance tree-size))))))


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


(defun score-similarity (target-func raw-student-solution instructor-solutions)
  "Return the similarity score (0 to 1.0) between the definition for function
   TARGET-FUNC present in the STUDENT-SOLUTION list and the one found in the 
   INSTRUCTOR-SOLUTIONS list. The latter is in the form 
   (doc (defun ...) ...) 

   Currently, a solution to a question consists of a single defun, with no
   helpers. This applies to the instructor's versions of the solution
   and to the student's solution"
  (labels ((filter-atoms (program)
             (cond ((null program) program)
                   ((atom (car program)) (filter-atoms (cdr program)))
                   (t (cons (car program) (filter-atoms (cdr program))))))
           (get-relevant-code (program call-graph)
             (let ((form-map (make-hash-table :test 'eq)))
               ;; Index the program once: O(N)
               (dolist (form program)
                 (setf (gethash (second form) form-map) form))

               (append
                ;; Filter constants: O(N)
                (remove-if-not (lambda (f)
                                 (and (consp f)
                                      (member (car f) '(defconstant defparameter defvar))))
                               program)
                ;; Map call-graph nodes to forms: O(M)
                (loop for (node-name has-target) in call-graph
                      when (and has-target (gethash node-name form-map))
                        collect (gethash node-name form-map)))))
           (embed-helpers (main-func-name program)
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
                   program))))
    (let* ((student-solution (filter-atoms raw-student-solution))
           (student-solution-cg (get-call-graph target-func student-solution))
           (student-used-functions-and-globals
             (get-relevant-code student-solution student-solution-cg))
           (student-sols-with-embedded-helpers (embed-helpers target-func student-used-functions-and-globals))
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
              (let ((similarity-score (similarity instructor-solution student-sols-with-embedded-helpers)))
                (when (or (null max-similarity)
                          (> similarity-score (first max-similarity)))
                  (setf max-similarity (list similarity-score instructor-solution student-sols-with-embedded-helpers)))))
            instructor-sols-with-embedded-helpers)
      max-similarity)))
