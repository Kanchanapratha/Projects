#| Calculates the maximum matching of the biparte pairs and finds the correct location 
USCID: 9940188502
Name : Kanchanapratha Rasappan
Email :rasappan@usc.edu |#

;;;-----------------place-knights : Find the maximum possible locations for the knights on the grid
;;;Input : size and unused
;;;Output: valid positions 
(defun place-knights ( inList)
(cond 
		((null inList) "Please specify input")
		((or (not (listp inList))(not(numberp (caar inList))) (not(numberp (cadar inList))))"The input format is not correct")
		((null (cadr inList)) (print-alternatives (caar inList) (cadar inList)))
		(t (knight-locations inList))))

;;; -----------------Knight-locations
(defun knight-locations ( inList &key (par '((-1 -1))) (chi '((-2 -2))))
(let ((n (caar inList))(m (cadar inList)) (unused (cadr inList)) (parent (copy-list par))(child (copy-list chi)))
(let (( pairs (find-all-Match n m 1 1 unused :parent parent :child child)))
   (let (( good (find-final-set n m 1 1 unused (append (car pairs) (cadr pairs)))))
      (let ((good2 (get-good-ones n m (car pairs) (cadr pairs) (get-used good) (* 3 (length (car pairs))))))  
    (append (get-only-good good) good2 ))))))
	
;;;---------------Print-alternatives
(defun print-alternatives (n m &key (p1 1)(p2 1))
(cond
		((> p1 n) nil)
		((> p2 m) (print-alternatives n m :p1 (1+ p1) :p2 1))
		((= (logand (logxor p1 p2) 1) 0) (cons (list p1 p2 ) (print-alternatives n m :p1 p1 :p2 (1+ p2))))
		(t (print-alternatives n m :p1 p1 :p2 (1+ p2)))))
		
		
;;;------------ Get parent : Get the parent of the child
;;; Input : location, parent list 
;;; Output : sublist at the specified location
(defun get-parent (location parent &optional (index 0))
(if (= index location) (car parent) (get-parent location (cdr parent) (1+ index)))
) 

;; DEF little long
;;;------------Get-matches : find the attacking positions for a given point
;;; Input : Coordinate x , y , size of the grid n , m
;;; Output: List of positions in the grid
(defun get-matches (n m px1 py1)
(remove nil(maplist #'(lambda (x) 
					(if (and (> (+ (caar x) px1) 0) 
						(<= (+ (caar x) px1) n) 
						(> (+ (cadar x) py1) 0) 
						(<= (+ (cadar x) py1) m))
                        (list (+ (caar x) px1) (+ (cadar x) py1)) nil))
					    '((-2 1)(-1 2)(1 2)(2 1)(2 -1)(1 -2)(-1 -2)(-2 -1)))))

						
;;;------------------ manage-pairs : matches the parent and the child of the biparte pair
;;;Input : Parent to be managed , child to be managed and the parent and child list
;;;Output : Parent and child list 
(defun manage-pairs (parentPoint childPoint parent child)
	(let ((parentPosition (position parentPoint parent :test #'equal)))
		(if (null parentPosition)
			(let ()(nconc parent (list parentPoint)) (nconc child (list childPoint)))
			(let () (replace child (list childPoint) :start1 parentPosition :end1 (1+ ParentPosition) :start2 0))
		)
	)
)

;;;; DEF little  long	
;;;-------------------Get-max-pairs : find the maximum matching in the biparte grid
;;;Input : size n m , point, parent and child list and the unused blocks
;;; Output : 1 or 0 and (computed parent and child list)										
(defun get-max-pairs (n m point parent child unused) 
(cond
((not (null (position point unused :test #'equal))) 0)
(t
(loop 
	for match in (get-matches n m (car point) (cadr point)) 
	do
	(let ((unusedState (position match unused :test #'equal))(childState (position match child :test #'equal)))
	    (if (= (compute-children n m unusedState childState match parent child point unused) 1) (return-from get-max-pairs 1) nil)
	))0
)))

;; DEF little long
;;;-----------------Compute-children : finds the children for the parent and appends or replaces the value in the list 
;;;Input : size n m , point, parent and child list and the unused blocks
;;;Output : 1 or 0 and (computed parent and child list)
(defun compute-children ( n m unusedState childState match parent child point unused)
(cond
		((and (null unusedState) (null childState)) (let() (manage-pairs point match parent child) 1))
		((and (null unusedState) (not (null childState)))
			(let ((parentLocation (position match child :test #'equal)))
			(let (( parentPoint (get-parent parentLocation parent)))
				(IF (= 1 (get-max-pairs n m parentPoint parent child (cons match unused)))
					(let() (manage-pairs point match parent child) 1) 0)
			)))
		(t 0)))

;;DEF somewhat long
;;;-----------------find-all-match : finds all the maximum matching in the biparte pairs
;;;Input : grid size and unused blocks
;;;Output : biparte matches
(defun find-all-Match ( x y p1 p2 unused &key (parent '((-1 -1))) (child '((-2 -2))))
(cond
		((> p1 x) (list (cdr parent) (cdr child)))
		((> p2 y) (find-all-Match x y (1+ p1) 1 unused :parent parent :child child))
		((= (logand (logxor p1 p2) 1) 1)
			(let () (get-max-pairs x y (list p1 p2) parent child unused)(
					find-all-Match x y p1 (1+ p2) unused :parent parent :child child)))
		(t (find-all-Match x y p1 (1+ p2) unused :parent parent :child child))
))
 
;;;-----------------get-used : Seperates two lists
;;;Input : list
;;;Output : seperated list
(defun get-used (good)
	(if (null(cdr good)) (car good) (get-used (cdr good))))


;;;-----------------get-only-good : gets the first part of the list
;;;Input : List
;;;Output :seperated list
(defun get-only-good(good)
(cond
		((null(cdr good)) nil)
		(t (cons (car good)(get-only-good (cdr good))))))
	
	
;;DEF Somewhat long
;;;-----------------find-all-match : finds all the maximum matching in the biparte pairs
;;;Input : grid size and unused blocks
;;;Output : biparte matches
(defun find-final-set ( n m p1 p2 unused pairs)
(cond
		((> p1 n) (list unused))
		((> p2 m) (find-final-set n m (1+ p1) 1 unused pairs))
		((and (null (position (list p1 p2) pairs :test #'equal)) (null (position (list p1 p2) unused :test #'equal)))
			(cons (list p1 p2) (find-final-set  n m p1 (1+ p2) (union unused (get-matches n m p1 p2) :test #'equal) pairs)))
		(t (find-final-set n m p1 (1+ p2) unused pairs))
))
 
 
 ;;;-----------------Valid-combi : checks if the combination of points is valid or not
;;;Input : combination, grid size n m
;;;Output: 1 if the combination is valid or 0
(defun valid-combi (tempComb n m &optional combi)
(cond
		((null combi) (valid-combi tempComb n m tempComb))
		((null tempComb) 1)
		((not (null (intersection combi (get-matches n m (caar tempComb) (cadar tempComb)) :test #'equal))) 0)
		(t
		(valid-combi (cdr tempComb) n m combi))
))

;;;DEF somewhat long, append
;;;-----------------find-combi : gets all the combination possible in the remaining points
;;;Input : matches (set1 and set2 ), grid size
;;;Output: Valid combination of position
(defun find-comb (set1 set2 n m &optional (value 1))
(cond
		((> value (expt 2 (length set1))) nil) 
		((= value (+ (length set1) 3)) (find-comb set2 set1 n m (1+ value)))
		((<= value 2)
			(if (= (valid-combi set1 n m) 1) set1 (find-comb set2 set1 n m (1+ value) )))
		((= (valid-combi (cons (car set1)(cdr set2)) n m) 1) (list (car set1)(cdr set2)))
		(t (find-comb (nconc (cdr set1)(list(car set1))) (nconc (cdr set2)(list(car set2))) n m (1+ value)))))
 
;;;DEF somewhat long, append 
;;;-----------------get-good-ones : gets the best in each pair
;;;Input : pair (set1 and set2) , grid size, unused
;;;Output: valid position in each pair
(defun get-good-ones (n m set1 set2 unused loopVal)
(let((validSet (get-good-sets (car set1) (car set2) unused)))
(cond 
		((null set1) nil)
		((= loopVal 0)(find-comb set1 set2 n m))
		((not(null validSet)) (cons validSet (get-good-ones n m (cdr set1) (cdr set2) (union unused (get-matches n m (car validSet) (cadr validSet)) :test #'equal) (1- loopVal))))
		(t (get-good-ones n m (nconc (cdr set1) (list(car set1)))  (nconc (cdr set2)(list(car set2))) unused (1- loopVal)))
))) 

;;;-----------------get-good-sets : check if any one of the position is in unsed
;;;Input : positions and unused
;;;Output: valid position 
(defun get-good-sets (point1 point2 unused)
(cond
		((and (null(position point1 unused :test #'equal)) (not (null (position point2 unused :test #'equal)))) point1)
		((and (null(position point2 unused :test #'equal)) (not (null (position point1 unused :test #'equal)))) point2)
		(t nil)))


	
	



