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
  (if (premisseRespecte? x)
    (format t "~%Nom de règle : ~a~%premisse : ~a~%Nouveau Faits : ~a~%" (caddr x) (cadr x) (car x))
    )
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
  (cond
    ((EQUAL (caar (getPremisse regle)) 'EGALITE)
        (format t "BASE DE FAIT = ~a~%REGLE = ~a~%~%" (caadr (car (getPremisse regle))) (caddr (car (getPremisse regle))))
        (EQUAL (cadr (ASSOC (caadr (car (getPremisse regle))) *BaseFaits*)) (cadr (cadr (getPremisse regle))))
      )
    ((EQUAL (caar (getPremisse regle)) 'DEFINI) NIL)
    ((EQUAL (caar (getPremisse regle)) 'COMPARAISON) NIL)
    )
)
(ajouterFait (list 'TYPEDEROCHE 'Micachiste))
(ajouterFait (list 'TYPEDEPIOCHE 'Double))
(TESTREGLES)
