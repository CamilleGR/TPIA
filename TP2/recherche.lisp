(load "successeurs.lisp")



;;;; --------------------------------------------------------------------
;;;;                        MYMEMBER
;;;;
;;;; Cette fonction permet de vérifier que la liste x est bien membre de la 
;;;; liste l. c'est tout simplement une autre implémentation de la méthode
;;;; member qui utilise EQUAL à la place de EQL
;;;;
;;;; ---------------------------------------------------------------------



(defun myMember (x l)
  (if (not (null l))
      (if (equal x (car l)) l (myMember x (cdr l)))))




;;;; --------------------------------------------------------------------
;;;;                        RECHERCHE
;;;;
;;;; Méthode qui permet de faire une recherche en profondeur dans notre 
;;;; graphe d'état. Cette fonction prend en paramètre l'état de départ
;;;; et l'état final avec le chemin parcouru en paramètre optionnel
;;;;
;;;; ---------------------------------------------------------------------


(DEFUN recherche ( etatCourrant etatFinal &optional etatsParcourus) ;;;; etatsParcourus est optionnel
  (dotimes (i (list-length etatsParcourus)) ;;;; On fait la boucle autant de fois qu'il y'a détats parcourus
    (format t "~T"))                        ;;;; On affiche une tabulation 
  (format t "~A~%" etatCourrant)            ;;;; On affiche en suite l'état courrant
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant)) ;;;; Si l'état courrant est l'état final, alors on a trouvé le chemin 
      (LET ((list-succ (successeurs etatCourrant)) (sol nil)) ;;;; Déclaration de nos variables
	(DO ((succ (pop list-succ) (pop list-succ))) ;;;; succ va prendre la valeur de chaque successeur possible
	    ((OR (null succ) (not (null sol))) sol) ;;;; On continue tant qu'il y a des successeurs possibles et que nous n'avons pas de solutions
	  (IF (AND (NOT (myMember succ etatsParcourus)) (not (null succ))) ;;;; si l'état n'a pas déjà été parcouru
	      (SETQ sol (recherche succ etatFinal (append etatsParcourus (list etatCourrant)))) ;;;; on rappelle notre fonction recherche
	      )
	  )
	)
      )
  )
