;; codemeter.lisp

(in-package #:codemeter)

(defun gen-nrhs (v nv bds)
  (if (null bds) nil
      (cons (list (caar bds) (chng-varname v (cadar bds) nv))
            (gen-nrhs v nv (cdr bds)))))

(defun chng-varname (var e nvar)
  (cond ((null e) nil)
        ((and (atom e) (eq var e)) nvar)
        ((atom e) e)
        ((eq (car e) 'let)
         (let ((nrhs (gen-nrhs var nvar (cadr e))))
           (cons 'let (chng-bds nrhs (cddr e)))))
        ((listp (car e)) (cons (chng-varname var (car e) nvar)
                               (chng-varname var (cdr e) nvar)))
        (t (cons (chng-varname var (car e) nvar) (chng-varname var (cdr e) nvar)))))

(defun chng-vars (bds nbds bdy)
  (cond ((null bds) bdy)
        ((atom (car bds)) (chng-vars (cons (list (car bds) nil) (cdr bds)) nbds bdy))
        (t (let ((nbdy (chng-varname (caar bds) bdy (caar nbds))))
             (chng-vars (cdr bds) (cdr nbds) nbdy)))))

(defun gen-new-vars (bds)
  (cond ((null bds) nil)
        ((atom (car bds)) (gen-new-vars (cons (list (car bds) nil) (cdr bds))))
        (t (cons (list (gensym) (cadar bds)) (gen-new-vars (cdr bds))))))

(defun chng-bds (bds bdy)
  (let ((nbds (gen-new-vars bds)))
    (list nbds (chng-vars bds nbds bdy))))

(defun var-mentioned? (v e)
  (cond ((null e) nil)
        ((and (atom e) (eq v e)) t)
        ((atom e) nil)
        ((listp (car e)) (or (var-mentioned? v (car e)) (var-mentioned? v (cdr e))))
        (t (or (var-mentioned? v (car e)) (var-mentioned? v (cdr e))))))

(defun referred-vars (bds e)
  (cond ((null bds) 0)
        ((var-mentioned? (caar bds) e) (1+ (referred-vars (cdr bds) e)))
        (t (referred-vars (cdr bds) e))))

(defun prsize (e)
  "Returns the size (in atoms) of expression e. The size of a LET expression
   is the same as its LET-less equivalent expression. "
  (cond ((null e) 0)
        ((atom e) 1)
        ((eq (car e) 'let)
         (let ((ne (chng-bds (cadr e) (cddr e))))
           (+ (prsize (car ne)) (prsize (cdr ne)) (- (* (referred-vars (car ne) (cdr ne)) 2)))))
        (t (+ (prsize (car e)) (prsize (cdr e))))))

(defun contains-forbidden-function? (e)
  (cond ((null e) nil)
        ((atom e) (car (member e *forbidden-functions*)))
        ((eq (car e) 'defun) (contains-forbidden-function? (cddr e)))
        (t (or (contains-forbidden-function? (car e)) (contains-forbidden-function? (cdr e))))))

(defun program-size (filename &optional baseline)
  "Returns the number of atoms (any object that is not a cons) occurring in lisp program file
   subtracted the atoms appearing in the baseline program.
   Returns NIL if the program mentions a forbidden function"
  ;; read-file-forms  returns a list containing the forms present in the file
  (let* ((plist (uiop:read-file-forms filename))
         (psize (prsize plist))
         (bsize (if baseline (prsize (uiop:read-file-forms baseline))
                    0)))
    (list (contains-forbidden-function? plist)
          (- psize bsize))))


(defun compare-progs (std-sol lab-sol &optional baseline)
  "Receives the student's solution file, the lab solution file, and an optional baseline file
   containing the code shared with the students. Returns how much smaller (bigger) the student's solution
   is compared to the lab solution. A negative value means the student's solution is longer. "
  (let* ((bs (if baseline (program-size baseline) 0))
         (std (program-size std-sol))
         (sol (program-size lab-sol))
         (nstd (- std bs))
         (nsol (- sol bs))
         (dif (- nsol nstd))
         (p% (/ (* dif 100.0) nsol)))
    p%))
    
    
