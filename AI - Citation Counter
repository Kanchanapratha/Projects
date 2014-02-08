(defun count-citations-for-all(names database)
(cond 
((null (car names)) nil)
(t (cons (list (car names) (count-citations (car names) database)) (count-citations-for-all (cdr names) database)))
))

(defun count-citations (name database)
(let ((authorwork (get-articles name database)))
	(let ((articles (get-article-names authorwork)))
		(compute-score (car (get-cited-articles articles database)) name (get-coauthors name authorwork) 0)
)))

(defun get-articles (name database)
(remove nil (maplist #'(lambda (x) 
	(unless (null 
		(position name (caddar x) :test #'equal)) (car x))) 
         database)
))

(defun get-article-names (articles)
(maplist #'(lambda (x) (caar x)) articles)
)

(defun get-cited-articles(article-names database)
(remove nil (maplist #'(lambda (x) 
	(unless (null 
		(intersection article-names (car (cddddr (car x))) 
			:test #'equal)) x)) 
         database)
))


(defun compute-score (cited-articles name coauthors score)
(cond 
((null cited-articles) score)

((= 1 (citation-by-self (car cited-articles) name)) 
	(compute-score (cdr cited-articles) name coauthors score))

((and (= 1 (citation-by-coauthor (car cited-articles) coauthors)) 
	(= 1 (citation-after (car cited-articles) 1995)))
	(compute-score (cdr cited-articles) name coauthors (1+ score)))

((= 1 (citation-by-coauthor (car cited-articles) coauthors))
	(compute-score (cdr cited-articles) name coauthors (+ 2 score)))

((= 1 (citation-after (car cited-articles) 1995))
	(compute-score (cdr cited-articles) name coauthors (+ 6 score)))

(t (compute-score (cdr cited-articles) name coauthors (+ 4 score)))

	)
)

(defun citation-by-self (article name)
(cond
((null (position name (caddr article) :test #'equal)) 0)
(t 1)
))

(defun citation-by-coauthor (article coauthors)
(cond
((null (intersection coauthors (caddr article) :test #'equal)) 0)
(t 1)
))

(defun citation-after (article year)
(cond
((> (cadr article) year) 1)
(t 0)
))


(defun get-coauthors (name articles)
(remove nil (maplist #'(lambda(x) 
	(car(remove name (caddar x)))) articles))

)

