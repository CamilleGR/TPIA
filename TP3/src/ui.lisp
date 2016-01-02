(load "moteur_inference.lisp")
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



(defun nainVsUtc()
  (format t "~%Bien sure que oui, t'en a d'autre des questions cons ?~%")
  )

(defun GestionDesNains()
  (format t "~%Parce que c'est des ivrognes pardi !~%")
  )

(defun gestionDesNains()
  (let ((longueurTunnel)(largeurTunnel)(nbJourMax)(hauteurTunnel))
    (loop while (NULL longueurTunnel) do
      (format t "~%De quel longueur est votre tunnel ?")
      (setq longueurTunnel (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL longueurTunnel) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE") )
    )
    (loop while (NULL largeurTunnel) do
      (format t "~%De quel largeur est votre tunnel ?")
      (setq largeurTunnel (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL largeurTunnel) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE") )
    )
    (loop while (NULL nbJourMax) do
      (format t "~%Dans combien de temps votre tunnel doit être prêt ?")
      (setq nbJourMax (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL nbJourMax) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE") )
    )
    (loop while (NULL hauteurTunnel) do
      (format t "~%Dans combien de temps votre tunnel doit être prêt ?")
      (setq hauteurTunnel (parse-integer (string (read-line)) :junk-allowed t))
      (if (NULL hauteurTunnel) (format t "ERREUR : VEUILLEZ ENTRER UN NOMBRE") )
    )
    (ajouterFait (list 'LargeurTunnel LargeurTunnel))
    (ajouterFait (list 'longueurTunnel longueurTunnel))
    (ajouterFait (list 'NombreDeJours nbJourMax))
    (ajouterFait (list 'hauteurTunnel hauteurTunnel))
  ;  (ajouterFait (list 'TypeDePioche 'Double))
    (ajouterFait (list 'TypeDeRoche 'GRANITE))
    (print *BaseFaits*)
    (chainageAvant)
    (if (cadr (assoc 'ChantierRéalisable *BaseFaits*))
      (format t "~%~%LE CHANTIER EST REALISABLE ~%~%")
      (format t "~%~%LE CHANTIER N'EST PAS REALISABLE ~%~%")
    )
    (format t "~%~%~c[31m~a~c[0m~%~%" #\ESC *BaseFaits* #\ESC)
  )
)


(defun menu()
   (loop
    (let ((choix))
      ;(shell "clear")
      (format t "~%~%          GESTION DU PERSONNEL MINIER EN ZONE DE CREUSEMENT INTENSIF~%~%")
      (format t "~%")
      (format t "    1- Combien me faut il de nain pour creuser mon tunnel ?~%")
      (format t "    2- Un nain boit-il plus qu'un UTCéen ?~%")
      (format t "    3- Pourquoi gérer les nains c'est difficile ?~%")
      (format t "    0- Quitter~%~%")
      (setq choix (string (read-line)))
      (cond ((EQUAL choix "1") (gestionDesNains))
	    ((EQUAL choix "2") (nainVsUtc))
	    ((EQUAL choix "3") (GestionDesNains))
	    ((EQUAL choix "0") (return-from menu "À la prochaine !"))
	    (T (warn "Faudrait penser à apprendre à lire, ce que t'as rentré c'est pas bon ! ~%"))
	    )
      )
    )
   )
(menu)
