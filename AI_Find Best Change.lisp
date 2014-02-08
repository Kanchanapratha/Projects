;;---------------------Finds the best change
;;Optional inputs : Cents and Coin List
;;Output : Minimum number of coins used

(defun find-best-change (&optional cents coinList (minVal 0))
    (cond ((null cents) " please specify a value for cents")
	      ((null coinList) "Please specify a set of coins")
		  ((or (not (listp coinList)) (not (numberp cents)))"Input format is not correct")
		  ((and (< cents 100) (= minVal 0)) (find-best-change cents (remove-duplicates  coinList) (get-count coinList (list cents))))
		  (t
		  (get-best-sol (remove-duplicates  coinList) cents minVal))))
		  
;;---------------------get-best-sol : gets the solution 
;;Optional inputs : Cents and Coin List
;;Output : minimum number of soins and list

(defun get-best-sol (coinList cents minVal)	
(let (( bestSol (make-change (sort (build-good-coinlist cents coinList) #'>) cents 0 minVal)))
					(append (format-output (car bestSol) (cadr bestSol)) (append-unused-coins coinList cents (position cents coinList)))))				

;;---------------------append-unused-coins : the coins greater than the cent value are appended
;;Optional inputs : Cents and Coin List
;;Output : Coins appened with zero
					
(defun append-unused-coins (coinList cents &optional appendAll)
        (cond
		((null coinList) nil)
		((and(not (null appendAll))(not(= (car coinList) cents))) (cons (list 0 (car coinList)) (append-unused-coins (cdr coinList) cents appendAll)))
		((> (car coinList) cents) (cons (list 0 (car coinList)) (append-unused-coins (cdr coinList) cents appendAll)))
		(t
		(append-unused-coins (cdr coinList) cents appendAll)))
)

;;;----------------------Format-Output : formats the output	
;;Input : list of coins and list of values
;;;Output : list of coins and values formatted
	  
(defun format-output (list1 list2)
              (cond
               ((null list2) nil )
               (t
                  (cons (list (car list1) (car list2)) (format-output (cdr list1)(cdr list2))))))
				  
;;;-----------------------Make-change : checks the minimum value out of all possible combinations
;;Input : permutedlist , cents and optional results 
;;Output : the best result out of all combi
 		
(defun make-change (coinList cents count minVal &optional result)
		(let ((perm (coerce (permute count coinList) 'list)))
		(let ((bestCoins (get-coins cents perm)))
        (cond
			((or (= (factorial (length coinList)) count)(and (> minVal 0) (= (apply #'+ (car result)) minVal))) result)
			((or (< (apply #'+ bestcoins) (apply #'+ (car result))) (null result)) (make-change coinList cents (1+ count) minVal (list bestcoins perm)))
			(t
			(make-change coinList cents (1+ count) minVal result))))))
			
;;----------------------factorial: calculates the factorial of a number
;;Input : number
;;Output : factorial
			
(defun factorial (n)
   (if (<= n 1)
       1
       (* n (factorial (1- n)))))
			
;;----------------------Permute : Obtains all possible ways
;;Input : CoinList
;;Output : permutations
			
(defun permute (k coinList)
    (let ((coins  (make-array (list (length coinList)) :initial-contents coinList)) (j nil))
               (dotimes (i (length coins) coins)
      (multiple-value-setq (k j)
        (floor k (- (length coins) i)))
      (rotatef (svref coins i) (svref coins (+ i j))) )))


;;------------------------get-coins : get the number of coins used for each permutation
;; Input : amount and the coinList
;; output : number of each coins used 

(defun get-coins (x lst)
  (cond ((null x) nil)
        ((and (null lst) (= x 0)) nil)
	  ((and (null lst) (> x 0)) (cons (* x 1000)(get-coins 0 (cdr lst))))
		((= x 0) (cons 0 (get-coins 0 (cdr lst))))
        (t (let ((l (multiple-value-bind (y z)
                        (truncate x (car lst))
                      (list y z))))
             (cons (car l) (get-coins (cadr l) (cdr lst)))))))
			 
;;------------------------Build-good-coinlist - Remove all coin values above the cents
;; Input : cents and coinList
;;Output : coinList
			 
(defun build-good-coinlist (x lst &optional tempList)
	(cond ((null lst) tempList)
		  ((< (car lst) x) (build-good-coinlist x (cdr lst) (cons (car lst) tempList)))
		  ((= (car lst) x) (build-good-coinlist x nil (list (car lst))))
		  (t
		  (build-good-coinlist x (cdr lst) tempList)
		  )))

;;------------------------get-values : get the minimum coins by dynamic programming
;; Input : cents and coinList
;;Output : minimum coins		  
		  
(defun get-values (coinList valueList)
(cond
((null valueList) nil)
(t
(nconc (remove nil(mapcar #'(lambda (x) (if (<= x (car valueList)) (- (car valueList) x) nil)) coinList)) (get-values coinList (cdr valueList)))
)))

;;------------------------get-values : get the minimum coins by dynamic programming
;; Input : coinList and valueList
;;Output : valuelist (new)	
(defun get-count (coinList valueList &optional (sum 0))
(cond
((not (null (position 0 valueList ))) sum)
((or (null valueList) (> sum 5))  -1)
(t
(get-count coinList (remove nil (get-values coinList valueList)) (1+ sum)))
))
