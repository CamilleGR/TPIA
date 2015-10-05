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

;;; Mymember

;;; Définition d'une méthode similaire à member mais utilisant EQUAL à la place de EQL

(defun myMember (x l)
  (if (not (null l))
      (if (equal x (car l)) l (myMember x (cdr l)))))

;;; Intersection récursive

(defun inter (l1 l2) (if (null (car l1)) () (if (mymember (car l1) l2)
			 (append (list (car l1)) (inter (cdr l1) l2))
			 (inter (cdr l1) l2)) ))

;;; Question 2 Iteratif

(defun inter-iteratif (l1 l2)
  (mapcan #'(lambda (x) (if (mymember x l2) (list x))) l1))

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
  (if (mymember (car l1) (cdr l1))
      (elim (cdr l1))
    (append (list (car l1)) (elim (cdr l1))))))

;;; Question 3 Itératif

(defun elim-ite (l1)
  (let ((listResult ()))
    (dolist (x l1 listResult)
      (if (and (mymember x l1) (not (mymember x listResult)))
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
(nbFeuilles '( a b c (d e) nil ))

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




;;; Exercice 2 : Solution de base (itérative avec mapcar)
(defun list-paire (x y)
               (if (and (listp x) (listp y))
                 (mapcar #'(lambda (x y) (list x y)) x y)
                 "Les deux arguments doivent être des listes !"
                 )
)


;;; Seconde solution : recursivite
;;;Les deux arguments sont les deux listes.
(defun list-paire-récursif (x y)
  (if (not (or (null (cdr x)) (null (cdr y))))
  	(cons (list (car x) (car y)) (list-paire-récursif (cdr x)      (cdr y)))
     	(list (list (car x) (car y)))
  )
)


;;; Jeu de test

(list-paire '(0 2 3 11) '(6 10 20 30))
(list-paire-récursif '(0 2 3 11) '(6 10 20 30))
(list-paire '(0 2 3 11 15) '(6 10 20 30))
(list-paire-récursif '(0 2 3 11 15) '(6 10 20 30))






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

;;; Exercice 4

;;; Définition de la base de connaissance


(setq BaseTest '((" Le Dernier Jour d'un condamné " Hugo 1829 50000)
(" Notre-Dame de Paris " Hugo 1831 3000000)
(" Les Misérables " Hugo 1862 2000000)
("Le Horla " Maupassant 1887 2000000)
(" Contes de la bécasse " Maupassant 1883 500000)
("Germinal " Zola 1885 3000000)
))


;;; A 1
(defun auteur (ouvrage)
  (cadr ouvrage))

;;; A 2
(defun titre (ouvrage)
  (car ouvrage))

;;; A 3
(defun annee (ouvrage)
  (caddr ouvrage))

;;; A 4
(defun nombre (ouvrage)
  (cadddr ouvrage))

;;; B 1 
(defun affichAllOuvrage (base)
  (mapcar #'(lambda (l) (titre l)) base))

;;; B 2 
(defun affichAllOuvrageOfHugo (base)
  (remove nil (mapcar #'(lambda (l) (if (EQ (auteur l) 'Hugo) (titre l) ())) base)))

;;; B 3

(defun affichAllOuvrageOfAutor (base autor)
  (remove nil (mapcar #'(lambda (l) (if (EQ (auteur l) autor) (titre l) ())) base)))

;;; B 4

(defun affichFirstOuvrageOf (base year)
  (dolist (l base) (if (EQ (annee l) year) (return (titre l)))))

;;; B 5
(defun affichOuvrageWithMore100k (base)
  (remove nil (mapcar #'(lambda (l) (if (>= (nombre l) 100000) (titre l) ())) base)))

;;; B 6
defun moyenne (base aut)
  (let ((nbExemplaire 0.0)(sum 0.0))
   (/ (dolist (livre base sum)
   (if (equal aut (auteur livre))
     (progn 
     (setq nbExemplaire (+ nbExemplaire 1))
     (setq sum (+ sum (cadddr livre)))))
   ) nbExemplaire )))

;;; Jeux de tests :
(affichAllOuvrage BaseTest)
(affichAllOuvrageOfHugo BaseTest)
(affichAllOuvrageOfAutor BaseTest ‘Maupassant)
(affichFirstOuvrageOf BaseTest 1831)
(affichOuvrageWithMore100k BaseTest)
(moyenne BaseTest ‘Maupassant)
