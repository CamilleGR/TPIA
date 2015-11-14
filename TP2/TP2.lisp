

(defun get_symbol (etat x)
  (cond
   ((OR (EQUAL x 0) (EQUAL etat NIL)) NIL)
   ((EQUAL x 1) (car etat))
   (T  (get_symbol (cdr etat) (- x 1)))
  )
)

(defun echange(etat x y)
  (if (AND (> x 0) (> y 0) (< x 5) (< y 5))
  (let ((value ()))
    (dotimes (i 5 value)
      (cond
       ((= i x) (setq value (append value (list (get_symbol etat y)))))
       ((= i y) (setq value (append value (list (get_symbol etat x)))))
       ((> i 0) (setq value (append value (list (get_symbol etat i)))))
       )
      )
    )
  (error "Les pièces sont positionnées entre 1 et 4"))
  )

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


;;;;--------------------------------------------------------------------------
;;;; etat_correct (e)
;;;; @param :
;;;; e : c'est un état donc une liste des pions avec leur position (A B C D)
;;;;
;;;;
;;;; Fonction qui permet de savoir si un etat est correct ou non
;;;; On estime qu'un etat est correct si et seulement si :
;;;; - Celui ci à seulement 4 membres qui sont A B C et D
;;;; - Si il n'a pas 2 membres identiques
;;;; - Si le A est positionné plus à gauche que le D
;;;;
;;;;---------------------------------------------------------------------------
(defun etat_correct(e)
  (if (AND (> (list-length (member 'A e)) (list-length (member 'D e)))
       (= (list-length e) 4)
       (member 'A e)
       (member 'B e)
       (member 'C e)
       (member 'D e)) T NIL))


(defun successeurs (etat)
  (let ((value ()))
  (loop for i in (list 1 3 4)
    do (if (etat_correct (echange etat i 2))
	    (push (echange etat i 2) value)))
	value)
  )




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





(defun distance (etatA etatB)
  (let ((dist 0) (x (pop etatA)) (y (pop etatB)))
    (loop for i from 0 to 4 do
	  (if (AND (NOT (= i 1))(NOT (EQUAL x y)))
	      (setq dist (1+ dist)))
	  (setq x (pop etatA))
	  (setq y (pop etatB))
	  )
    dist
    )
  )


(defun choixEtat (etat liste parcouru)
  (let ((minDist)(minEtat))
    (dolist (e liste minEtat)
      ;;;;(format t "~t~tCHOIX ETAT : ~a -> ~a~%" e  (successeurs e))
      ;;;;(format t "~t~tminDist = ~a~%~t~tminEtat = ~a~%" minDist minEtat)
      (if (AND (OR (AND (NULL minDist) (NULL minEtat)) (< (distance etat e) minDist))
	       (> (list-length (successeurs e)) 1)
	       (NOT (MYMEMBER e parcouru)))
	  (progn
	    (setq minDist (distance etat e))
	    (setq minEtat e)
	    )
	)
      )
    ;;;;(format t "~%~t~t~tON CHOISI ~a~%" minEtat)
    minEtat
    )
  )


(DEFUN recherche_opti ( etatCourrant etatFinal &optional etatsParcourus) ;;;; etatsParcourus est optionnel
  (dotimes (i (list-length etatsParcourus)) ;;;; On fait la boucle autant de fois qu'il y'a détats parcourus
    (format t "~T"))                        ;;;; On affiche une tabulation 
  (format t "~A~%" etatCourrant)            ;;;; On affiche en suite l'état courrant
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant)) ;;;; Si l'état courrant est l'état final, alors on a trouvé le chemin
      (let ((etatPrometteur (choixEtat etatFinal (successeurs etatCourrant) etatsParcourus)))
      	(if (NOT (MYMEMBER etatPrometteur etatsParcourus))
		  (SETQ sol (recherche_opti etatPrometteur etatFinal (append etatsParcourus (list etatCourrant))))
	)
      )
      )
  )
