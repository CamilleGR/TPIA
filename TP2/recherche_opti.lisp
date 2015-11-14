(load "successeurs.lisp")
(load "distance.lisp")







;;;; --------------------------------------------------------------------
;;;;                        RECHERCHE
;;;;
;;;; Méthode qui permet de faire une recherche en profondeur dans notre 
;;;; graphe d'état. Cette fonction prend en paramètre l'état de départ
;;;; et l'état final avec le chemin parcouru en paramètre optionnel
;;;;
;;;; ---------------------------------------------------------------------


(DEFUN recherche_opti_liste ( etatCourrant etatFinal &optional etatsParcourus) ;;;; etatsParcourus est optionnel
  (dotimes (i (list-length etatsParcourus)) ;;;; On fait la boucle autant de fois qu'il y'a détats parcourus
    (format t "~T"))                        ;;;; On affiche une tabulation 
  (format t "~A~%" etatCourrant)            ;;;; On affiche en suite l'état courrant
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant)) ;;;; Si l'état courrant est l'état final, alors on a trouvé le chemin 
      (LET ((list-succ (plusProche etatFinal (successeurs etatCourrant))) (sol nil)) ;;;; Déclaration de nos variables
	(DO ((succ (pop list-succ) (pop list-succ))) ;;;; succ va prendre la valeur de chaque successeur possible
	    ((OR (null succ) (not (null sol))) sol) ;;;; On continue tant qu'il y a des successeurs possibles et que nous n'avons pas de solutions
	  (IF (AND (NOT (myMember succ etatsParcourus)) (not (null succ))) ;;;; si l'état n'a pas déjà été parcouru
	      (SETQ sol (recherche_opti_liste succ etatFinal (append etatsParcourus (list etatCourrant)))) ;;;; on rappelle notre fonction recherche
	      )
	  )
	)
      )
  )



(DEFUN recherche_opti ( etatCourrant etatFinal &optional etatsParcourus etatIncorrect) ;;;; etatsParcourus est optionnel
  (dotimes (i (list-length etatsParcourus)) ;;;; On fait la boucle autant de fois qu'il y'a détats parcourus
    (format t "~T"))                        ;;;; On affiche une tabulation 
  (format t "~A incorrect : ~%" etatCourrant etatIncorrect)            ;;;; On affiche en suite l'état courrant
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant)) ;;;; Si l'état courrant est l'état final, alors on a trouvé le chemin
      (let ((etatPrometteur (choixEtat etatFinal (successeurs etatCourrant) etatIncorrect)))
      (if (NOT (MYMEMBER etatPrometteur etatsParcourus))
	  (SETQ sol (recherche_opti etatPrometteur etatFinal (append etatsParcourus (list etatCourrant)) etatIncorrect))
	(SETQ sol (recherche_opti etatPrometteur etatFinal etatsParcourus (append etatIncorrect (list etatPrometteur))))
	)
      )
      )
  )
