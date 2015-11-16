
;;;; --------------------------------------------------------------------
;;;;                        GET_SYMBOL
;;;;
;;;; Cette fonction permet de donner le x ème symbole de etat
;;;;
;;;; Exemple : (get_symbol '(A B C D) 2)
;;;;           >B
;;;;
;;;; ---------------------------------------------------------------------



(defun get_symbol (etat x)
  (cond
   ((OR (EQUAL x 0) (EQUAL etat NIL)) NIL)
   ((EQUAL x 1) (car etat))
   (T  (get_symbol (cdr etat) (- x 1)))
  )
  )

;;;; --------------------------------------------------------------------
;;;;                        ECHANGE
;;;;
;;;; Cette fonction permet d'échanger le x ème symbole et le y ème symbole
;;;; de l'état etat.
;;;;
;;;; ---------------------------------------------------------------------

(defun echange(etat x y)
  (if (AND (> x 0) (> y 0) (< x 5) (< y 5))
  (let ((value ()))
    (dotimes (i 5 value)
      (cond
       ((= i x) (setq value (append value (list (get_symbol etat y))))) ;;;; Si le symbole est x alors on met y
       ((= i y) (setq value (append value (list (get_symbol etat x))))) ;;;; Si le symbole est y alors on met x
       ((> i 0) (setq value (append value (list (get_symbol etat i))))) ;;;; Sinon on met le bon symbole
       )
      )
    )
  (error "Les pièces sont positionnées entre 1 et 4")) ;;;; Gestion des erreurs potentielles
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

(defun etat_correct(e) ;;;; Un etat est correct si 
  (if (AND (> (list-length (member 'A e)) (list-length (member 'D e))) ;;;; A est avant D 
       (= (list-length e) 4)  ;;;; Si l'état comporte exactement 4 symboles
       (member 'A e)          ;;;; Si A est membre de l'état
       (member 'B e)          ;;;; Si B est membre de l'état
       (member 'C e)          ;;;; Si C est membre de l'état
       (member 'D e)) T NIL)) ;;;; Si D est membre de l'état


;;;; --------------------------------------------------------------------
;;;;                        SUCCESSEURS
;;;;
;;;; Fonction qui permet de renvoyer les successeurs valides d'un état
;;;; etat.
;;;;
;;;; ---------------------------------------------------------------------

(defun successeurs (etat)
  (let ((value ()))
  ;;;; On fait les échanges 1 <-> 2, 2 <-> 3 et 2 <-> 4
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


;;;; --------------------------------------------------------------------
;;;;                        DISTANCE
;;;;
;;;; La distance entre deux états peux être définie par le nombre de 
;;;; symboles qu'il n'ont pas en commun. En effet, plus ils ont de symboles
;;;; différents, plus les états sont éloigné.
;;;;    Avec l'énnoncé, nous ométerons le deuxième symbole qui bougera à 
;;;; chaque changement d'états, ce qui fait qu'il n'est pas pertinent
;;;; de le compter dans la distance.
;;;; ---------------------------------------------------------------------

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

;;;; --------------------------------------------------------------------
;;;;                        CHOIXETAT
;;;;
;;;; Un etat est considéré comme étant le meilleur état si il respecte 
;;;; plusieurs conditions :
;;;;    - Sa distance avec l'état final est la plus petite.
;;;;    - Il n'a pas déjà été parcouru.
;;;;    - Il possède d'autres états successeurs que son prédecesseur
;;;;
;;;; ---------------------------------------------------------------------


(defun choixEtat (etat liste parcouru)
  (let ((minDist)(minEtat))
    (dolist (e liste minEtat)
      (if (AND
	   (OR (AND (NULL minDist) (NULL minEtat)) (< (distance etat e) minDist)) ;;;; Si la distance entre e et minEtat est plus petite
	   (NOT (MYMEMBER e parcouru)))        ;;;; ET si e n'a pas déja été parcouru
	  (progn
	    (setq minDist (distance etat e))   ;;;; ALORS e est un état plus prometteur que minEtat
	    (setq minEtat e)
	    )
	)
      )
    minEtat ;;;; On retourne le meilleur état trouvé
    )
  )

;;;; --------------------------------------------------------------------
;;;;                        RECHERCHE_OPTI
;;;;
;;;; Voici notre nouvel algorithme de recherche qui selectionne à chaque
;;;; fois l'état le plus proche de notre état d'arrivée.
;;;;
;;;; ---------------------------------------------------------------------

  
  (DEFUN recherche_opti ( etatCourrant etatFinal &optional etatsParcourus) ;;;; etatsParcourus est optionnel
  (if (NOT (AND (etat_correct etatCourrant) (etat_correct etatFinal)))
      (error "ERREUR : ENTREE(S) INCORRECT(S)")
      )
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant)) ;;;; Si l'état courrant est l'état final, alors on a trouvé le chemin
	  (let ((etatPrometteur (choixEtat etatFinal (successeurs etatCourrant) etatsParcourus))) ;;;; On choisi l'état le plus prometteur
	      	(loop while (not (null etatPrometteur))
		      do ;;;; Si l'état le plus prometteur n'a pas déjà été parcouru on continu notre exploration
		      (if (NOT (MYMEMBER etatPrometteur etatsParcourus)) 
				  (SETQ sol (recherche_opti etatPrometteur etatFinal (append etatsParcourus (list etatCourrant))))
				)
				(if (null sol) ;;;; Si la solution est nul, on essaye avec un autre successeur.
					(setq etatPrometteur (choixEtat etatFinal (remove etatPrometteur (successeurs etatCourrant)) etatsParcourus))
					(setq etatPrometteur NIL)
				)
		 )
    sol)
  )
)
 
;;;;-----------------------------------------------------------------
;;;;
;;;;                         TESTS
;;;;
;;;;-----------------------------------------------------------------



(defparameter *etatsPossibles* '((A B C D) (A B D C) (A C B D)
				 (A C D B) (A D C B) (B A C D)
				 (B A D C) (B C A D) (C A B D)
				 (C A D B) (C B A D) (A D B C)))

(defun test_recherche_opti ()
  (dolist (e *etatsPossibles* 'OK)
    (dolist (et *etatsPossibles* 'OK)
      (format t "~% ~a et ~a ~%" e et)
      (let (( resultat (recherche_opti e et)))
	(if (NULL resultat) (error "La recherche n'a donné aucune solutions"))
	(format t "~a~%" resultat)
	)
      )
    )
  )

(defun comparaison_performances ()
  (let ((nbOpti 0.0)(nbNaif 0.0)(nbAll 0.0)(nbEgalite 0.0))
  (dolist (e *etatsPossibles* 'OK)
    (dolist (et *etatsPossibles* 'OK)
      (let (( resultatOpti (list-length (recherche_opti e et )))(resultatNaif (list-length (recherche e et))))
	(cond ((< resultatOpti resultatNaif) (setq nbOpti (1+ nbOpti)))
	      ((> resultatOpti resultatNaif) (setq nbNaif (1+ nbNaif)))
	      (T (setq nbEgalite (1+ nbEgalite))))
	(setq nbAll (1+ nbAll))
	)
      )
    )
  (format t "~%CALCUL SUR ~a ÉLÉMENTS~%TAILLES EGALES = ~a~%HEURISTIQUE PLUS OPTIMISE = ~a ~%HEURISTIQUE MOINS OPTIMISE = ~a~%"
	  nbAll nbEgalite nbOpti nbNaif)
   (format t "~%POURCENTAGES~%TAILLES EGALES = ~a %~%HEURISTIQUE PLUS OPTIMISE = ~a % ~%HEURISTIQUE MOINS OPTIMISE = ~a %~%"
	  (* (/ nbEgalite nbAll) 100) (* (/ nbOpti nbAll) 100) (* (/ nbNaif nbAll) 100))
   )
  )

