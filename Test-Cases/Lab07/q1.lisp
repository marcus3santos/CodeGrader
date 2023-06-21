;; q1.lisp

(defstruct ht
  array
  (count 0)
  comp)

(defun ht-create (kvs &key (test 'eql))
  (let ((res (make-ht :array (make-array 16 :initial-element nil)
                      :comp test)))
    (loop :for (k  v) :in kvs :do
          (ht-add k v res))
    res))

(deftest test-ht-delete ()
  (check
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
	     (ht-delete 4 *ht*)
	     (list (ht-get 4 *ht*) (ht-count *ht*)))  ; accessing a deleted item
	   '(nil 5))
    (equal (let ((*ht* (ht-create '((-5 -5) (1  1) (2  2) (3  3) (4  4) (5  5)))))
             (ht-delete 4 *ht*)          ; ensuring deleted kv has been marked
             (ht-get 5 *ht*))
           5)
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
	     (ht-delete 4 *ht*)
	     (ht-delete 4 *ht*))  ; deleting an already deleted item
	   nil)
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)) :test 'equal)))
	     (ht-add "a" 44 *ht*)
	     (and (= (ht-delete "a" *ht*) 44)
		  (= (ht-count *ht*) 6)))
	   t)))
					

(deftest test-ht-add ()
  (check
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
             (ht-add 2 2 *ht*))            ; inserting duplicate
	   nil)
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
	     (ht-delete 4 *ht*)
	     (ht-add 4 4 *ht*)) ; inserting a previously deleted item
	   '(4 . 4))
    (equal (let* ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)) :test 'equal)))
	     (ht-add "String" "Not-a-Symbol" *ht*))
	   '("String" . "Not-a-Symbol"))
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
	     (ht-add 'String "Symbol" *ht*))
	   '(String . "Symbol"))))

(deftest test-ht-get ()
  (check
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
	     (ht-get 7 *ht*))     ; getting a non existing item
	   nil)
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))))
	     (ht-get 6 *ht*))   ; getting an existing item
	   6)
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) ("String" "Not-a-symbol")) :test 'equal)))
	     (ht-get "String" *ht*))
	   "Not-a-symbol")
    (equal (let ((*ht* (ht-create '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (String "Symbol")))))
	     (ht-get 'String *ht*))
	   "Symbol")))

(defun test-q1 ()
  (test-ht-delete)
  (test-ht-add)
  (test-ht-get)
  (fmakunbound 'ht-delete)
  (fmakunbound 'ht-add)
  (fmakunbound 'ht-get))

(test-q1)
