(load "successeurs.lisp")


(defun myMember (x l)
  (if (not (null l))
      (if (equal x (car l)) l (myMember x (cdr l)))))


(DEFUN recherche ( etatCourrant etatFinal &optional etatsParcourus)
  (dotimes (i (list-length etatsParcourus))
    (format t "~T"))
  (format t "~A~%" etatCourrant)
  (IF (EQUAL etatCourrant etatFinal)
      (append etatsParcourus (list etatCourrant))
      (LET ((list-succ (successeurs etatCourrant)) (sol nil))
	(DO ((succ (pop list-succ) (pop list-succ)))
	    ((OR (null succ) (not (null sol))) sol)
	  (IF (AND (NOT (myMember succ etatsParcourus)) (not (null succ)))
	      (SETQ sol (recherche succ etatFinal (append etatsParcourus (list etatCourrant))))
	      )
	  )
	)
      )
  )
