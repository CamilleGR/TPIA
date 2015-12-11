;;; Definition de la base de règles

(setq *baseDeRegles* '( (F (B D E) R1)
		      (A (D G) R2)
		      (A (C F) R3)
		      (D (C) R4)
		      (E (D) R5)
		      (H (A) R6)
		      (X (B) R7)
		      (A (X C) R8))
      )

(defun etatFinal (regle) (car regle))
(defun premisse (regle) (cadr regle))
(defun nomRegle (regle) (caddr))

(defun regles_candidates (baseDeRegles fait)
  (let ((sol ()))
    (dolist (x baseDeRegles sol)
      (if (EQUAL (etatFinal x) fait)
	  (setq sol (nconc (list x) sol))
	)
      )
    )
  )

;;;--------------------------------------------------------
;;;
;;;---------------- CHAINAGE ARRIERE ----------------------
;;;
;;;--------------------------------------------------------

;;; Verifier_ou => utiliser la méthode some, qui renvoie vrai si un élément de la liste est vrais

(defun verifier_ou(br bf but)
  (if (member but bf) T
    (some #'(lambda (x)
	      (format t "VERIFIE_OU ~A ~%" but)
	      (verifier_et br bf (premisse x))) (regles_candidates br but))
    )
  )

;;; Verifier_et => utiliser la méthode every, qui renvoie vrai si tous les éléments de la liste sont vrais

(defun verifier_et(br bf but)
  (format t "VERIFIE_ET ~A ~%" but)
  (every #'(lambda(x) (verifier_ou br bf x )) but))
