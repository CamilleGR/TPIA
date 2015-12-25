(load "base_de_regles.lisp")

(defparameter *BaseFaits* (list))

#|
    Definition des fonctions outils
|#

(defun afficheRegles()
  (dolist (x *BaseRegles* NIL)
    (format t "~%Nom de règle : ~a~%premisse : ~a~%Nouveau Faits : ~a" (caddr x) (cadr x) (car x))
  )
)

(defun TESTREGLES()
  (dolist (x *BaseRegles* NIL)
    ;;(format t "~%Nom de règle : ~a~%premisse : ~a~%Nouveau Faits : ~a~%" (caddr x) (cadr x) (car x))
      (premisseRespecte? x)
    )
  )

(defun ajouterFait(fait)
  (if (assoc (car fait) *BaseFaits*)
    (setf (assoc (car fait) *BaseFaits*) (cdr fait))
    (push fait *BaseFaits*)
  )
)

(defun getPremisse(regle)
  (cadr regle)
  )
(defun getNom(regle)
    (caddr regle)
  )
(defun getNouveauxFaits(regle)
    (car regle)
  )


(defun premisseRespecte?(regle)
  ;;;;(format t "(car (getPremisse regle)) = ~a~%" (car (car (getPremisse regle))))
  (let ((test T))
    (dolist ( x (getPremisse regle) test)
      (setq test (AND test (conditionRespecte? x)))
    )
  )
)


(defun conditionRespecte?(condition)
;;;;(format t "Condition = ~a~%Type =~a~%Valeurs = ~a~%"
;;;;  condition (car condition) (cadr (cadr condition)))
(cond
  ((EQUAL (car condition) 'EGALITE)
      (format t "~a == ~a = ~a~%~%" (cadr (ASSOC (car (cadr condition)) *BaseFaits*)) (cadr (cadr condition)) (EQUAL (cadr (ASSOC (car (cadr condition)) *BaseFaits*)) (cadr (cadr condition))))
      (EQUAL (cadr (ASSOC (cadr condition) *BaseFaits*)) (cadr (cadr condition)))
    )
  ((EQUAL (car condition) 'DEFINI)
    (format t "~a EST IL DEFINI ?? ~%" condition )
    (if (ASSOC (car (cadr condition)) *BaseFaits*)
        (progn
          (format t "OUI~%~%")
          T
      )
      NIL))
  ((EQUAL (car condition) 'COMPARAISON)
    (format t "COMPARAISON = ~a~%" condition)
    NIL)
  )
)
#|
  ~ = Alt + N
  | = Alt + Shift + L

  Pour la comparaison :
    - Si c'est un comparateur alors on le garde
    - Si c'est un opérateur on le garde
    - Si c'est un symbole on va chercher sa valeur dans la base de fait (il faut vérifier qu'il existe)
    - Si c'est une liste appel récursif

    Faire deux fonctions :
      - Une fonction qui va vérifier la comparaison  "verifierComparaison"
      - Une fonction qui va évaleur toutes les opérandes "evalOperande"
|#



(ajouterFait (list 'TYPEDEROCHE 'Micachiste))
(ajouterFait (list 'TYPEDEPIOCHE 'Double))
(ajouterFait (list 'VITESSENAIN 1))
(TESTREGLES)
(print *BaseFaits*)
