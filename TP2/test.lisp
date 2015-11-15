(load "TP2.lisp")


(defparameter *etatsPossibles* '((A B C D) (A B D C) (A C B D)
				 (A C D B) (A D C B) (B A C D)
				 (B A D C) (B C A D) (C A B D)
				 (C A D B) (C B A D) (A D B C)))

(defun test_recherche_opti ()
  (dolist (e *etatsPossibles* NIL)
    (dolist (et *etatsPossibles* NIL)
      (format t "~% ~a et ~a ~%" e et)
      (format t "~a~%" (recherche_opti e et))
      )
    )
  )

(defun test_recherche_opti_liste ()
  (dolist (e *etatsPossibles* 'OK)
    (dolist (et *etatsPossibles* 'OK)
      (format t "~% ~a et ~a ~%" e et)
      (let (( resultat (recherche_opti e et)))
	(if (NULL resultat) (error "La recherche n'a donné aucune solutions"))
	(format t "~a~%" resultat)
	)
      )
    )
  )


(defun test_recherche ()
  (dolist (e *etatsPossibles* 'OK)
    (dolist (et *etatsPossibles* 'OK)
      (format t "~% ~a et ~a ~%" e et)
      (let (( resultat (recherche e et)))
	(if (NULL resultat) (error "La recherche n'a donné aucune solutions"))
	(format t "~a~%" resultat)
	)
      )
    )
  )
