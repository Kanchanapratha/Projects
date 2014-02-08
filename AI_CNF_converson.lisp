(defun convert-to-CNF (input &optional vars)
(let ((input1 (remove-implications (make-logical input))))
  (case (get-car input1)
    ('NOT (let ((p2 (apply-negation (first-rest input1))))
	   (if (check-literal p2) p2 (convert-to-CNF p2 vars))))
    ('AND (create-conjunction (append-result #'(lambda (q) (obtain-conjuncts (convert-to-CNF q vars)))
				(rest-args input1))))
    ('OR  (combine-disjuncts (mapcar #'(lambda (q) (convert-to-CNF q vars))
				  (rest-args input1))))
    (t   input1) 
    )))
	
(defun obtain-disjuncts (sentence)
  (cond ((eql (get-car sentence) 'or) (rest-args sentence))
	((eql sentence 'fal) nil)
	(t (list sentence))))
						  
(defun second-rest (exp) (second (rest-args exp)))
						  
(defun create-exp (get-car &rest rest-args) (cons get-car rest-args))

(defsetf rest-args (exp) (new-value)
  `(setf (cdr ,exp) ,new-value))
  
(defun get-car (exp) (if (listp exp) (first exp) exp))

(defun first-rest (exp) (first (rest-args exp)))

(defun get-prefix (string)		
    (if (stringp string)
      (with-input-from-string (stream (concatenate 'string "#I(" string ")"))
	(read stream))
      string))
	  
(defun rest-args (exp) (if (listp exp) (rest exp) nil))
 
 (defun make-logical (sentence)
    (cond ((stringp sentence) (get-prefix sentence))
	(t sentence)))
	
 (defun append-result (fn &rest lists)
  (reduce #'append (apply #'mapcar fn lists) :from-end t))
  
(defun apply-negation (pl)
  (case (get-car pl)
    ('NOT (first-rest pl))
    ('AND (create-disjunction (mapcar #'apply-negation (rest-args pl))))
    ('OR  (create-conjunction (mapcar #'apply-negation (rest-args pl))))
    (t (create-exp 'not pl))))
	
(defun remove-implications (pl)
  (if (check-literal pl)
      pl
    (case (get-car pl)
      (IF `(or ,(second-rest pl) (not ,(first-rest pl))))
      (IFF `(and (or ,(second-rest pl) (not ,(first-rest pl))) (or ,(first-rest pl) (not ,(second-rest pl)))))
      (t   (cons (get-car pl) (mapcar #'remove-implications (rest-args pl)))))))
	
(defconstant +connectives+ '(and or not IF IFF))

(defun check-literal (sentence)
  (or (check-atomic sentence)
      (and (eql (get-car sentence) 'not) (check-atomic (first-rest sentence)))))
	  
(defun check-atomic (sentence)
  (not (member (get-car sentence) +connectives+)))
  
  (defun create-disjunction (rest-args)
    (case (length rest-args)
    (0 'fal)
    (1 (first rest-args))
    (t (cons 'or rest-args))))
	
(defun obtain-conjuncts (sentence)
  (cond ((eql (get-car sentence) 'and) (rest-args sentence))
	((eql sentence 'tru) nil)
	(t (list sentence))))
	
(defun create-conjunction (rest-args)
  (case (length rest-args)
    (0 'tru)
    (1 (first rest-args))
    (t (cons 'and rest-args))))
	

(defun combine-disjuncts (disjuncts)
    (case (length disjuncts)
    (0 'fal)
    (1 (first disjuncts))
    (t (create-conjunction
	(loop for y in (obtain-conjuncts (combine-disjuncts (rest disjuncts))) append
	      (loop for  x in (obtain-conjuncts (first disjuncts)) 
		    collect (create-disjunction (append (obtain-disjuncts x)
						  (obtain-disjuncts y)))))))))
						  

