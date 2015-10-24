
;;;;--------------------------------------------------------------------------
;;;; etat_correct (e)
;;;; @param :
;;;; e : c'est un état donc une liste des pions avec leur position (A B C D)
;;;;
;;;;
;;;; Fonction qui permet de savoir si un etat est correct ou non
;;;; On estime qu'un etat est correct si et seulement si :
;;;; - Celui ci à seulement 4 membres qui sont A B C et D
;;;; - Si il n'a pas 2 membres identiques
;;;; - Si le A est positionné plus à gauche que le D
;;;;
;;;;---------------------------------------------------------------------------
(defun etat_correct(e)
  (if (AND (> (list-length (member 'A e)) (list-length (member 'D e)))
       (= (list-length e) 4)
       (member 'A e)
       (member 'B e)
       (member 'C e)
       (member 'D e)) T NIL))
