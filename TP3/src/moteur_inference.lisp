(load "base_de_regles.lisp")

(defparameter *BaseFaits* (list))

#|
    Definition des fonctions outils
|#

(defun afficheRegles()
  (dolist (x *BaseRegles* NIL)
    (format t "~%~%Nom de règle : ~a~%premisse : ~a~%Nouveau Faits : ~a" (caddr x) (cadr x) (car x))
  )
)

(defun AjouterFait(fait)
  (if (assoc (car fait) *BaseFaits*)
    (setf (assoc (car fait) *BaseFaits*) (cdr fait))
    (push fait *BaseFaits)
  )
)

(afficheRegles)
