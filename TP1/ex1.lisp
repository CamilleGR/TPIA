;;; Exercice 1

;;; 4 x 3 – 5 x 2 + 3 x + 1
;;; (+ (* 4 x x x) (* -5 x x) (* 3 x) 1 )


;;; Question 1
(defun firstn (n l)
  (if (> n 0) (append (list (car l)) (firstn (- n 1) (cdr l)))))

;;; Question 1 ( iteratif )

(defun firstn-ite (n l)
  (let ((listeEntiere l))
    (setq liste ())
  (dotimes (x n liste)
    (setq liste (append liste (list (car listeEntiere))))
    (setq listeEntiere (cdr listeEntiere))
    )))

;;; Jeu de test

(firstn 3 '( 1 5 6 4 8))
(firstn 3 '( 1 5))
(firstn 3 '( 1 5 6 4 8))
(firstn 3 '( 1 5 6 4 8))





;;; Question 2
(defun inter (l1 l2) (if (null (car l1)) () (if (member (car l1) l2)
			 (append (list (car l1)) (inter (cdr l1) l2))
			 (inter (cdr l1) l2)) ))

;;; Question 2 Iteratif

(defun inter-iteratif (l1 l2)
  (mapcan #'(lambda (x) (if (member x l2) (list x))) l1))

;;; Jeu de test
(inter '(1 2 3) '())
(inter '(1 2 3) '(2 3))
(inter '(1 2 3 (4 5 6)) '(1 2 3 (4 5 6)))
(inter-iteratif '(1 2 3) '())
(inter-iteratif '(1 2 3) '(2 3))
(inter-iteratif '(1 2 3 (4 5 6)) '(1 2 3 (4 5 6)))






;;; Question 3
(defun elim (l1)
(if (null (car l1)) ()
  (if (member (car l1) (cdr l1))
      (elim (cdr l1))
    (append (list (car l1)) (elim (cdr l1))))))

;;; Question 3 Itératif

(defun elim-ite (l1)
  (let ((listResult ()))
    (dolist (x l1 listResult)
      (if (and (member x l1) (not (member x listResult)))
	  (setq listResult (cons x listResult))))))

;;; Jeu de test
(elim '(1 2 3 4 5 6 6 5 4 3 2))
(elim '(1 2 3 4))
(elim '(1 2 3 4 (1 2) (1 2)))
(elim-ite '(1 2 3 4 (1 2) (1 2)))
(elim-ite '(1 2 3 4 5 6 6 5 4 3 2))
(elim-ite '(1 2 3 4))

;;; Question 4
(defun nbfeuilles (l1) (if (null (car l1))
			   0
			 (if (listp (car l1)) (+ (nbfeuilles (car l1)) (nbfeuilles (cdr l1))) (+ 1 (nbfeuilles (cdr l1))))))

;;; Jeu de test

(nbFeuilles '( r (( t )) y ( g h ) ( j m l ) p ))
(nbFeuilles '( a b c ))
'( a b c (d e) nil ))

;;; Question 5

(defun monEqual (e1 e2)
  (cond (( and (listp e1) (listp e2) (not (null e1)) (not (null e2)))
	       (and (monEqual (car e1) (car e2)) (monEqual (cdr e1) (cdr e2))))
	((and (atom e1) (atom e2))
	                (eq e1 e2))
	((and (null e1) (null e2))
	                T)
	('T 'nil)))

;;; Jeu de test

(monequal 'LUC 'LUC)
(monequal 'LUC 'DANIEL)
(monequal '(d p f t r) '(d p f t r))
(monequal (car '(do re)) (cadr '(mi do sol)))


;;; Exercice 3

(defun my-assoc (list elem)
  (setq value nil)
  (dolist (x list value)
    (if ( equal (car x) elem) (setq value (cdr x)))))

;;; Jeu de test

(setq base '((a 1) (b 2) (c 3) (abc (1 2 3)) (abab ((1 2) (1 2)))))
(my-assoc base 'a)
(my-assoc base 'b)
(my-assoc base 'c)
(my-assoc base 'abab)
(my-assoc base 'abc)
(my-assoc base 'notHere)



;;; Mymember


(defun myMember (x l)
  (if (not (null l))
      (if (equal x (car l)) l (myMember x (cdr l)))))
