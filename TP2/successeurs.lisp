(load "echange.lisp")
(load "etat-correct.lisp")

(defun successeurs (etat)
  (let ((value ()))
  (dotimes (i 4 value)
    (if (NOT (= i 1))
	(if (etat_correct (echange etat (+ i 1) 2))
	    (push (echange etat (+ i 1) 2) value)))))
)

