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
	    (setq minDist (distance etat e))
	    (push e minEtat)
	    )
	)
      )
    )
  )


(defun choixEtat (etat liste)
  (let ((minDist (distance etat (car liste)))(minEtat (car liste)))
    (dolist (e liste minEtat)
      (if (<= (distance etat e) minDist)
	  (progn
	    (setq minDist (distance etat e))
	    (setq minEtat e)
	    )
	)
      )
    )
  )
