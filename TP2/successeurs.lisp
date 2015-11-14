(load "echange.lisp")
(load "etat-correct.lisp")


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


(defun successeurs (etat)
  (let ((value ()))
  (dotimes (i 4 value)
    (if (NOT (= i 1))
	(if (etat_correct (echange etat (+ i 1) 2))
	    (push (echange etat (+ i 1) 2) value)))))
)
#|
(defun successeurs (etat)
  (let ((value ()))
  (loop for i in (list 1 3 4)
    do (if (etat_correct (echange etat i 2))
	    (push (echange etat i 2) value)))
	value)
)|#


