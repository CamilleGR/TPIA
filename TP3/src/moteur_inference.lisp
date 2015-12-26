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

(defun ajouterFait(fait)
  (if (assoc (car fait) *BaseFaits*)
    (setf (assoc (car fait) *BaseFaits*) (cdr fait))
    (push fait *BaseFaits*)
  )
)

(defun estUnComparateur? (symbole)
  (OR (EQUAL symbole '<) (EQUAL symbole '>) (EQUAL symbole '<=) (EQUAL symbole '>=))
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
      ;;;;(print x)
      (setq test (AND test (conditionRespecte? x)))
    )
  )
)


(defun conditionRespecte?(condition)
(cond
  ((EQUAL (car condition) 'EGALITE)
      ;;;;(format t "~a == ~a = ~a~%~%" (cadr (ASSOC (car (cadr condition)) *BaseFaits*)) (cadr (cadr condition)) (EQUAL (cadr (ASSOC (car (cadr condition)) *BaseFaits*)) (cadr (cadr condition))))
      (return-from conditionRespecte? (EQUAL (cadr (ASSOC (car (cadr condition)) *BaseFaits*)) (cadr (cadr condition)))))
  ((EQUAL (car condition) 'DEFINI)(return-from conditionRespecte? (NOT (NULL (ASSOC (car (cadr condition)) *BaseFaits*)))))
  ((EQUAL (car condition) 'COMPARAISON)
    (return-from conditionRespecte?
      (eval
        (let ((expression))
          (dolist (x (cadr condition) expression)
            (cond
              ((estUnComparateur? x)
                (setq expression (append expression (list x))))
                ((listp x)
                  (if (evalOperande x)
                    (setq expression (append expression (list (eval (evalOperande x)))))
                      NIL
                    ))
                (T
                  (if (ASSOC x *BaseFaits*)
                    (setq expression (append expression (list (cadr (ASSOC x *BaseFaits*)))))
                    NIL
                    ))
            )
          )
        ))))
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
      - Une fonction qui va évaleur toutes les opérandes "evalOperande"   DONE BIATCH
|#


(defun evalOperande(operande)
  (let ((newList (list)))
      (dolist (x operande newList)
        (cond
          ((LISTP x)
            (setq newList (append newList (list (evalOperande x)))))
          ((OR (EQUAL x '*) (EQUAL x '/) (EQUAL x '-) (EQUAL x '+))
            (setq newList (APPEND newList (list x))))
          ((SYMBOLP x)
            (if (NULL (ASSOC x *BaseFaits*))
                (return-from evalOperande NIL) ;;;; SI LE SYMBOLE N'EST PAS DEFINI DANS LA BDF
              (setq newList (APPEND newList (cdr (ASSOC x *BaseFaits*))))
            ))
          (T (setq newList (APPEND newList (list x))))
        )
    )
  )
)
