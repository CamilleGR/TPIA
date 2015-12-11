
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


  


(defun ask-user()
  (string (read-char))
  )

(defun nainVsUtc()
  (format t "~%Bien sure que oui, t'en a d'autre des questions cons ?~%")
  )

(defun GestionDesNains()
  (format t "~%Parce que c'est des ivrognes pardi !~%")
  )

(defun menu()
   (loop
    (let ((choix))
      (format t "~%~%          GESTION DU PERSONNEL MINIER EN ZONE DE CREUSEMENT INTENSIF~%~%")
      (format t "~%")
      (format t "    1- Combien me faut il de nain pour creuser mon tunnel ?~%")
      (format t "    2- Un nain boit-il plus qu'un UTCéen ?~%")
      (format t "    3- Pourquoi gérer les nains c'est difficile ?~%")
      (format t "    0- Quitter~%~%")
      (setq choix (ask-user))
      (cond ((EQUAL choix "1") (format t "~%En construction ! Revien plus tard !~%"))
	    ((EQUAL choix "2") (nainVsUtc))
	    ((EQUAL choix "3") (GestionDesNains))
	    ((EQUAL choix "0") (return-from menu "À la prochaine !"))
	    (T (warn "~%Faudrait penser à apprendre à lire, ce que t'as rentré c'est pas bon ! ~%"))
	    )
      )
    )
   )
(menu)
