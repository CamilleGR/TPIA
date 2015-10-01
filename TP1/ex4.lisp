(setq BaseTest '((" Le Dernier Jour d'un condamné " Hugo 1829 50000)
(" Notre-Dame de Paris " Hugo 1831 3000000)
(" Les Misérables " Hugo 1862 2000000)
("Le Horla " Maupassant 1887 2000000)
(" Contes de la bécasse " Maupassant 1883 500000)
("Germinal " Zola 1885 3000000)
))


(defun moyenne (base aut)
  (let ((nbExemplaire 0.0)(sum 0.0))
(/ (dolist (livre base sum)
(if (equal aut (cadr livre))
(progn 
(setq nbExemplaire (+ nbExemplaire 1))
(setq sum (+ sum (cadddr livre)))))) nbexemplaire )))


(moyenne BaseTest 'Hugo)
(moyenne BaseTest 'Zola)
(moyenne BaseTest 'Maupassant)
(moyenne BaseTest 'test) -- Division par 0 = probleme

