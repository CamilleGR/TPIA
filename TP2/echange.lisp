
(setq jeu_test_echange '((A B C D) (B C A D) (C A D B)))


(defun get_etat (etat x)
  (cond
   ((OR (EQUAL x 0) (EQUAL etat NIL)) NIL)
   ((EQUAL x 1) (car etat))
   (T  (get_etat (cdr etat) (- x 1)))
  )
)

(defun echange(etat x y)
  (if (AND (> x 0) (> y 0) (< x 5) (< y 5))
  (let ((value ()))
    (dotimes (i 5 value)
      (cond
       ((= i x) (setq value (append value (list (get_etat etat y)))))
       ((= i y) (setq value (append value (list (get_etat etat x)))))
       ((> i 0) (setq value (append value (list (get_etat etat i)))))
       )
      )
    )
  (error "Les pièces sont positionnées entre 1 et 4"))
  )
