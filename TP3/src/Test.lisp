(load "base_de_regles.lisp")
(load "moteur_inference.lisp")
;;;;(* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain 2)

(ajouterFait (list 'TYPEDEROCHE  'Micachiste))
(ajouterFait (list 'TYPEDEPIOCHE  'Double))
(ajouterFait (list 'LargeurTunnel  5))
(ajouterFait (list 'HauteurTunnel  3))
(ajouterFait (list 'NombreDeJours  10))
(ajouterFait (list 'LongueurTunnel 10))
(ajouterFait (list 'VitesseNain  2))

(defun TESTREGLES()
  (dolist (x *BaseRegles* NIL)
      (format t "~a -> ~c[31m~a~c[0m~%" x #\ESC (premisseRespecte? x) #\ESC)
    )
  )

(defun TestAjoutNouveauxFaits()
  (dolist (x *BaseRegles* NIL)
    (format t "~%~%~a~%" (getNom x))
    (dolist (y (getNouveauxFaits x) NIL)
      (format t "~%~a" y)
      (format t "~%~a"  (eval (evaluerValeur (cdr y)))
    )
  )
)
)
;;;;(setq operandeTest '(* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain 2))
;;;;(print operandeTest)
;;(print (evalOperande operandeTest))|#
;;;(setq condition '(>=
;;;;  (* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain)
;;;;  LongueurTunnel
;;;;))
;;;;(let ((expression))
;;;;  (dolist (x condition expression)
;;;;    (if (estUnComparateur? x)
;;;;      (push x expression)
;;;;      (if (evalOperande x)
;;;;        (setq expression (append expression (list (evalOperande x))))
;;;;        NIL
;;;;        )
;;;;    )
;;;;    (format t "~a  ETAT LISTE = ~a ~%" x expression)
;;;;  )
;;;)

;;;;(TESTREGLES)

(setq  condition '(COMPARAISON (>=
  (* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain)
  LongueurTunnel))
  )

(setq valeur '(*
  (truncate (/ LargeurTunnel 1.25))
  HauteurTunnel))

(print (evaluerValeur valeur))
(print (eval (evaluerValeur valeur)))

(TestAjoutNouveauxFaits)
;;;;(TESTREGLES)
;;;;(print *BaseFaits*)
