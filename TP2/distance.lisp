;;;;----------------------------------------------------------
;;;;
;;;;    DISTANCE
;;;;
;;;;
;;;;
;;;;----------------------------------------------------------


(defun distance (etatA etatB)
  (let ((dist 0))
    (do ((x etatA (cdr x))(y etatB (cdr y)))
	((AND (NULL x) (NULL y)) dist)
     ;;;; (format t "~a et ~a~%" x y)
      (if (NOT (EQUAL (car x) (car y)))
	  (setq dist (1+ dist))
	)
      )
    )
  )


;;;;
;;;;
;;;;
;;;;

(defun plusProche (etat liste)
  (let ((minDist (distance etat (car liste)))(minEtat (list (car liste))))
    (dolist (e liste minEtat)
      (if (<= (distance etat e) minDist)
	  (progn
	    (print minDist)
	    (setq minDist (distance e etat))
	    (push e minEtat)
	    )
	)
      )
    )
  )
