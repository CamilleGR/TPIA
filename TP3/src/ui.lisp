(load "moteur_inference.lisp")
(load "detail_regles.lisp")
;;;;#########################################################################
;;;;
;;;;                  SOURCE CONTENANT L'INTERFACE UTILISATEUR
;;;;
;;;;#########################################################################
;;;;                                       ___I___
;;;;                                      /=  |  #\
;;;;                                     /.__-| __ \
;;;;                                     |/ _\_/_ \|
;;;;                                     (( __ \__))
;;;;                                  __ ((()))))()) __
;;;;                                ,'  |()))))(((()|# `.
;;;;                               /    |^))()))))(^|   =\
;;;;                              /    /^v^(())()()v^\'  .\
;;;;                              |__.'^v^v^))))))^v^v`.__|
;;;;                             /_ ' \______(()_____(   |
;;;;                        _..-'   _//_____[xxx]_____\.-|
;;;;                       /,_#\.=-' /v^v^v^v^v^v^v^v^| _|
;;;;                       \)|)      v^v^v^v^v^v^v^v^v| _|
;;;;                        ||       :v^v^v^v^v^v`.-' |#  \,
;;;;                        ||       v^v^v^v`_/\__,--.|\_=_/
;;;;                        ><       :v^v____|  \_____|_
;;;;                     ,  ||       v^      /  \       /
;;;;                    //\_||_)\    `/_..-._\   )_...__\
;;;;                   ||   \/  #|     |_='_(     |  =_(_
;;;;                   ||  _/\_  |    /     =\    /  '  =\
;;;;                    \\/ \/ )/     |=____#|    '=....#|



(defparameter *reglesUtilise* NIL)

(defun gestionDesNains()
  (let ((longueurTunnel)(largeurTunnel)(nbJourMax)(hauteurTunnel)(typeRoche)
          (listeRoche '(Gabbros Greis Micachiste Amphibolite Leptyrites
            Ophiolites Prasinites Serpentines Cipolins Splites Orthophyres Granite)))
    (loop while (OR (NULL longueurTunnel) (<= longueurTunnel 0)) do
      (format t "~%De quel longueur est votre tunnel ?")
      (setq longueurTunnel (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL longueurTunnel) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE POSITIF") )
    )
    (loop while (OR (NULL largeurTunnel) (<= largeurTunnel 0)) do
      (format t "~%De quel largeur est votre tunnel ?")
      (setq largeurTunnel (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL largeurTunnel) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE POSITIF") )
    )
    (loop while (OR (NULL hauteurTunnel) (<= hauteurTunnel 0)) do
      (format t "~%Quel est la hauteur de votre tunnel ?")
      (setq hauteurTunnel (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL hauteurTunnel) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE POSITIF") )
    )
    (loop while (OR (NULL nbJourMax) (<= nbJourMax 0)) do
      (format t "~%Dans combien de temps votre tunnel doit être prêt ?")
      (setq nbJourMax (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL nbJourMax) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE POSITIF") )
    )
    (loop while (OR (NULL typeRoche)(NOT (member typeRoche listeRoche))) do
      (format t "~%Selectionner le type de roche dans lequel vous voulez creuser :~%")
      (dolist (roche listeRoche) (format t "~a~%" roche))
      (setq typeRoche (intern (string-upcase (string (read-line)))))
      (if (OR (NULL typeRoche)(NOT (member typeRoche listeRoche)))
        (format t "ERREUR : VEUILLEZ ENTRER UNE ROCHE DE LA LISTE") )
    )

    (ajouterFait (list 'LargeurTunnel LargeurTunnel))
    (ajouterFait (list 'longueurTunnel longueurTunnel))
    (ajouterFait (list 'NombreDeJours nbJourMax))
    (ajouterFait (list 'hauteurTunnel hauteurTunnel))
    (ajouterFait (list 'TypeDeRoche typeRoche))
    (if (NULL (assoc 'TypeDePioche *BaseFaits*))
        (ajouterFait (list 'TypeDePioche 'Standard))
      )
    (print *BaseFaits*)
    (setq *reglesUtilise* (chainageAvant))
    (displayResult)
    (format t "~%~%~a~%~%" *reglesUtilise*)
  )
)

(defun displayResult()
  (cond
      ((NULL (cadr (assoc 'ChantierRéalisable *BaseFaits*)))
      (format t "~%--------------------------------------------------------------------~%")
      (format t "-----------     LE CHANTIER EST N'EST PAS REALISABLE     -----------~%")
      (format t "--------------------------------------------------------------------~%")
      )
      ((NULL (cadr (assoc 'EQUIPEDENUIT *BaseFaits*)))
        (format t "~%~%~%~%--------------------------------------------------------------------~%")
        (format t "--------------     LE CHANTIER EST REALISABLE     ------------------~%")
        (format t "--------------------------------------------------------------------~%")
        (format t "~%VOILÀ LA COMPOSITION DE VOS EQUIPES POUR FAIRE VOTRE CHANTIER LE ~%PLUS RAPIDEMENT POSSIBLE : ~%")
        (format t "~%VOUS AVEZ BESOIN D'UNE EQUIPE DE JOUR COMPOSÉE DE : ~%")
        (format t "Nains mineurs : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINMINIER *BaseFaits*)) #\ESC)
        (format t "Nains Guerisseurs : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINGUERISSEUR *BaseFaits*))#\ESC)
        (format t "Nains Forgerons : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINFORGERON *BaseFaits*)) #\ESC)
        (format t "Nains Tourneurs de manche : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINTOURNEURMANCHE *BaseFaits*))#\ESC)
        (format t "Nains Ravitailleurs : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINRAVITAILLEMENT *BaseFaits*))#\ESC)
        (format t "Nains Plongeurs (Vaisselle) : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINPLONGUEUR *BaseFaits*)) #\ESC)
        (format t "~%~%TOTAL :                                                ~c[31m~a~c[0m nains~%~%" #\ESC (cadr (assoc 'NbNainTotal *BaseFaits*)) #\ESC)
        (format t "--------------------------------------------------------------------~%~%~%")
      )
      (T
      (format t "~%~%~%~%--------------------------------------------------------------------~%")
      (format t "--------------     LE CHANTIER EST REALISABLE     ------------------~%")
      (format t "--------------------------------------------------------------------~%")
      (format t "~%VOILÀ LA COMPOSITION DE VOS EQUIPES POUR FAIRE VOTRE CHANTIER LE ~%PLUS RAPIDEMENT POSSIBLE : ~%")
      (format t "~%VOUS AVEZ BESOIN D'UNE EQUIPE DE JOUR COMPOSÉE DE : ~%")
      (format t "Nains mineurs : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINMINIER *BaseFaits*)) 2) #\ESC)
      (format t "Nains Guerisseurs : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINGUERISSEUR *BaseFaits*)) 2)#\ESC)
      (format t "Nains Forgerons : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINFORGERON *BaseFaits*)) 2)#\ESC)
      (format t "Nains Tourneurs de manche : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINTOURNEURMANCHE *BaseFaits*)) 2)#\ESC)
      (format t "Nains Ravitailleurs : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINRAVITAILLEMENT *BaseFaits*)) 2)#\ESC)
      (format t "Nains Plongeurs (Vaisselle) : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINPLONGUEUR *BaseFaits*)) 2)#\ESC)

      (format t "~%AINSI QUE D'UNE EQUPIDE DE NUIT COMPOSÉE DE : ~%")
      (format t "Nains Mineurs : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINMINIER *BaseFaits*)) 2) #\ESC)
      (format t "Nains Guerisseurs : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINGUERISSEUR *BaseFaits*)) 2)#\ESC)
      (format t "Nains Forgerons : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINFORGERON *BaseFaits*)) 2)#\ESC)
      (format t "Nains Tourneurs de manche : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINTOURNEURMANCHE *BaseFaits*)) 2)#\ESC)
      (format t "Nains Ravitailleurs : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINRAVITAILLEMENT *BaseFaits*)) 2)#\ESC)
      (format t "Nains Plongeurs (Vaisselle) : ~c[31m~a~c[0m~%" #\ESC (/ (cadr (assoc 'NBNAINPLONGUEUR *BaseFaits*)) 2)#\ESC)
      (format t "Nains Surveillants : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINSURVEILLANT *BaseFaits*)) #\ESC)
      (format t "Nains Managers : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINMANAGER *BaseFaits*))#\ESC)
      (format t "Nains Porteurs de lanternes : ~c[31m~a~c[0m~%" #\ESC (cadr (assoc 'NBNAINPORTEURLANTERNE *BaseFaits*))#\ESC)
      (format t "~%~%TOTAL :                                                ~c[31m~a~c[0m nains~%~%" #\ESC (cadr (assoc 'NbNainTotal *BaseFaits*)) #\ESC)
      (format t "--------------------------------------------------------------------~%~%~%")
      )
    )
    (return-from displayResult 'OK)
)

(defun ChangementDePioche()
  (let ((choix))
    (format t "~%Quel type de pioche vos nains vont ils utiliser ?~%1- Mauvaise Qualitée ~%2- Pioche standard~%3- Pioche Double~%4-Pioche en mithril~%")
    (setq choix (string (read-line)))
    (cond
      ((EQUAL choix "1") (ajouterFait (list 'TypeDePioche 'MauvaiseQualite)))
      ((EQUAL choix "2") (ajouterFait (list 'TypeDePioche 'Standard)))
      ((EQUAL choix "3") (ajouterFait (list 'TypeDePioche 'Double)))
      ((EQUAL choix "4") (ajouterFait (list 'TypeDePioche 'Mithril)))
      (T (warn "vous avez entré une valeur éronnée, vous utiliserez donc des pioches standards"))
      )
  )
)


(defun menu()
   (loop
    (let ((choix))
      ;(shell "clear")
      (format t "~%~%          GESTION DU PERSONNEL MINIER EN ZONE DE CREUSEMENT INTENSIF~%~%")
      (format t "~%")
      (format t "    1- Combien me faut il de nain pour creuser mon tunnel ?~%")
      (format t "    2- Changer l'equipement des nains~%")
      (if  *reglesUtilise* (format t "    3- Quelle règles ont été utilisées ?~%"))
      (format t "    0- Quitter~%~%")
      (setq choix (string (read-line)))
      (cond ((EQUAL choix "1") (gestionDesNains))
	    ((EQUAL choix "2") (ChangementDePioche))
	    ((AND (EQUAL choix "3") *reglesUtilise*)
          (dolist (r *reglesUtilise*)
            (format t "~%~a : ~a~%" r (cdr (assoc r *detailRegle*)))
          )
        )
	    ((EQUAL choix "0") (return-from menu "À la prochaine !"))
	    (T (warn "Faudrait penser à apprendre à lire, ce que t'as rentré c'est pas bon ! ~%"))
	    )
      )
    )
   )

(menu)
