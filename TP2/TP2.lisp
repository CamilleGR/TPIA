
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
  (loop for i in (list 1 3 4) ;;;; On fait les échanges 1 <-> 2, 2 <-> 3 et 2 <-> 4
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
  (let ((dist 0))
    (do ((x etatA (cdr x))(y etatB (cdr y)))
	((AND (NULL x) (NULL y)) dist)
      (if (NOT (EQUAL (car x) (car y)))
	  (setq dist (1+ dist))
	)
      )
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
	   (> (list-length (successeurs e)) 1) ;;;; ET si e possède d'autres états successeurs que son prédecesseur
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

(defun plusProche (etat liste)
  (let ((minDist (distance etat (car liste)))(minEtat (list (car liste))))
    (dolist (e liste minEtat)
      (cond ((= (distance etat e) minDist)
	     (push e minEtat)
	     )
	    ((<  (distance etat e) minDist)
	     (setq minDist (distance etat e))
	     (setq minEtat (list e)))
	    )
      )
    )
  )

;;;; --------------------------------------------------------------------
;;;;                        RECHERCHE_OPTI
;;;;
;;;; Voici notre nouvel algorithme de recherche qui selectionne à chaque
;;;; fois l'état le plus proche de notre état d'arrivée.
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


(DEFUN recherche_opti ( etatCourrant etatFinal &optional etatsParcourus) ;;;; etatsParcourus est optionnel
  (if (NOT (AND (etat_correct etatCourrant) (etat_correct etatFinal)))
      (error "ERREUR : ENTREE(S) INCORRECT(S)")
      )
  (dotimes (i (list-length etatsParcourus)) ;;;; On fait la boucle autant de fois qu'il y'a détats parcourus
    (format t "~T"))                        ;;;; On affiche une tabulation 
  (format t "~A~%" etatCourrant)            ;;;; On affiche en suite l'état courrant
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant)) ;;;; Si l'état courrant est l'état final, alors on a trouvé le chemin
      (let ((etatPrometteur (choixEtat etatFinal (successeurs etatCourrant) etatsParcourus))) ;;;; On choisi l'état le plus prometteur
      	(if (NOT (MYMEMBER etatPrometteur etatsParcourus)) ;;;; Si l'état le plus prometteur n'a pas déjà été parcouru on continu notre exploration
		  (SETQ sol (recherche_opti etatPrometteur etatFinal (append etatsParcourus (list etatCourrant))))
	)
      )
      )
  )
