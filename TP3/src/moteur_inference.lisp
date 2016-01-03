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


;;;;###############################################################################
;;;;
;;;;                    FONCTIONS DE VERIFICATIONS DES PREMISSES
;;;;
;;;;###############################################################################


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
  (let ((test T))
    (dolist ( x (getPremisse regle) test)
      (setq test (AND test (conditionRespecte? x)))
    )
  )
)

(defun evalOperande(operande)
  (let ((newList (list)))
      (dolist (x operande newList)
        (cond
          ((LISTP x)
            (setq newList (append newList (list (evalOperande x)))))
          ((OR (EQUAL x '*) (EQUAL x '/) (EQUAL x '-) (EQUAL x '+) (EQUAL x 'truncate) )
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

(defun conditionRespecte?(condition)
(cond
  ((EQUAL (car condition) 'EGALITE)
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


;;;;###############################################################################
;;;;
;;;;                 FONCTIONS DE MISE A JOUR DE LA BASE DE FAIT
;;;;
;;;;###############################################################################

;;;; ON MODIFIE LE FAIT SI IL EXISTE ET ON L'AJOUTE SI IL N'EXISTE PAS
(defun ajouterFait(fait)
  (if (assoc (car fait) *BaseFaits*)
    (setq *BaseFaits* (remove (car fait) *BaseFaits* :key #'first)))
    (push fait *BaseFaits*)
)

(defun evaluerValeur(val)
  (cond
    ((OR (EQUAL val T) (NULL val)(NUMBERP val)) val)
    ((SYMBOLP val) (cadr (ASSOC val *BaseFaits*)))
    ((LISTP val)
      (let (newVal)
          (dolist (x val newVal)
            (if (OR (EQUAL x '+) (EQUAL x '-) (EQUAL x '/) (EQUAL x '*) (EQUAL x 'truncate) (EQUAL x 'ceiling))
              (setq newVal (APPEND newVal (list x)))
              (setq newVal (APPEND newVal (list (evaluerValeur x))))
            )
          )
        )
    )
  )
)

(defun majBDF(regle)
  (dolist (x (getNouveauxFaits regle) NIL)
    (if (listp (evaluerValeur (cdr x)))
      (ajouterFait (list (car x) (eval (evaluerValeur (cdr x)))))
      (ajouterFait (list (car x) (cdr x)))
    )
  )
)


(defun chainageAvant()
  (let ((listeRegle *BaseRegles*)(nouveauxFaits T)(reglesUtilise NIL))
  (loop while nouveauxFaits do
    (setq nouveauxFaits NIL)
    (dolist (r listeRegle NIL) ;;;; TANT QU'IL Y A DES RÈGLES À TESTER
          (format t "~%REGLE = ~a~%BDF = ~a" (caddr r) *BaseFaits*)
            (if (premisseRespecte? r) ;;;; SI LES PREMISSES DE LA REGLE SONT RESPECTES
                (progn
                    (setq nouveauxFaits T)                      ;;;; ON NOTIFIE QU'IL Y A EUT UN NOUVEAU FAIT
                    (setq listeRegle (remove (caddr r) listeRegle :key #'third))  ;;;; ON REITIRE LA REGLE DE LA LISTE
                    (majBDF r)
                    (push (caddr r) reglesUtilise)
                )
            )
      )
  )
  reglesUtilise
)
)
