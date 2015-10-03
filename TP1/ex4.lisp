-- Exercice 2 : Solution de base (itérative avec mapcar)
(defun list-paire (x y)
               (if (and (listp x) (listp y))
                 (mapcar #'(lambda (x y) (list x y)) x y)
                 "Les deux arguments doivent être des listes !"
                 )
)

-- Solution deux : récursif sans mapcar

(defun list-paire-récursif (x y)
  (if (not (or (null (cdr x)) (null (cdr y))))
      (cons (list (car x) (car y)) (list-paire-récursif (cdr x) (cdr y)))
      (list (list (car x) (car y)))))
	  
-- Jeu de tests :
-- Normal :
(list-paire '(0 2 3 11) '(6 10 20 30))
(list-paire-récursif '(0 2 3 11) '(6 10 20 30))

-- Si différentes tailles :
(list-paire '(0 2 3 11 15) '(6 10 20 30))
(list-paire-récursif '(0 2 3 11 15) '(6 10 20 30))

(setq BaseTest '((" Le Dernier Jour d'un condamné " Hugo 1829 50000)
(" Notre-Dame de Paris " Hugo 1831 3000000)
(" Les Misérables " Hugo 1862 2000000)
("Le Horla " Maupassant 1887 2000000)
(" Contes de la bécasse " Maupassant 1883 500000)
("Germinal " Zola 1885 3000000)
))


-- Exercice 4
-- Initialisation de la base
(setq BaseTest '((" Le Dernier Jour d'un condamné " Hugo 1829 50000)
(" Notre-Dame de Paris " Hugo 1831 3000000)
(" Les Misérables " Hugo 1862 2000000)
("Le Horla " Maupassant 1887 2000000)
(" Contes de la bécasse " Maupassant 1883 500000)
("Germinal " Zola 1885 3000000)
))

-- A 1
(defun auteur (ouvrage)
  (cadr ouvrage))

-- A 2
(defun titre (ouvrage)
  (car ouvrage))

-- A 3
(defun annee (ouvrage)
  (caddr ouvrage))

-- A 4
(defun nombre (ouvrage)
  (cadddr ouvrage))

-- B 1 
(defun affichAllOuvrage (base)
  (mapcar #'(lambda (l) (titre l)) base))

-- B 2 
(defun affichAllOuvrageOfHugo (base)
  (remove nil (mapcar #'(lambda (l) (if (EQ (auteur l) 'Hugo) (titre l) ())) base)))

-- B 3

(defun affichAllOuvrageOfAutor (base autor)
  (remove nil (mapcar #'(lambda (l) (if (EQ (auteur l) autor) (titre l) ())) base)))

-- B 4

(defun affichFirstOuvrageOf (base year)
  (dolist (l base) (if (EQ (annee l) year) (return (titre l)))))

-- B 5
(defun affichOuvrageWithMore100k (base)
  (remove nil (mapcar #'(lambda (l) (if (>= (nombre l) 100000) (titre l) ())) base)))

-- B 6 (le tient Camille)
defun moyenne (base aut)
  (let ((nbExemplaire 0.0)(sum 0.0))
   (/ (dolist (livre base sum)
   (if (equal aut (auteur livre))
     (progn 
     (setq nbExemplaire (+ nbExemplaire 1))
     (setq sum (+ sum (cadddr livre)))))
   ) nbExemplaire )))

-- Jeux de tests :
(affichAllOuvrage BaseTest)
(affichAllOuvrageOfHugo BaseTest)
(affichAllOuvrageOfAutor BaseTest ‘Maupassant)
(affichFirstOuvrageOf BaseTest 1831)
(affichOuvrageWithMore100k BaseTest)
(moyenne BaseTest ‘Maupassant)
