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


(defun distance2 (etatA etatB)
  (let ((dist 0) (x (pop etatA)) (y (pop etatB)))
    (loop for i from 0 to 4 do
	  (if (AND (NOT (= i 1))(NOT (EQUAL x y)))
	      (setq dist (1+ dist)))
	  (setq x (pop etatA))
	  (setq y (pop etatB))
	  )
    dist
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


(defun choixEtat (etat liste etatsIncorrects)
  (let ((minDist)(minEtat))
    (dolist (e liste minEtat)
      (if (and (NOT (myMember e etatsIncorrects))  
      	(or (null minDist) (<= (distance etat e) minDist)))
	  (progn
	    (setq minDist (distance etat e))
	    (setq minEtat e)
	    )
	)
      minEtat)
    )
  )


(defun choixEtat2 (etat liste parcouru)
  (let ((minDist)(minEtat))
    (dolist (e liste minEtat)
      ;;;;(format t "~t~tCHOIX ETAT : ~a -> ~a~%" e  (successeurs e))
      ;;;;(format t "~t~tminDist = ~a~%~t~tminEtat = ~a~%" minDist minEtat)
      (if (AND (OR (AND (NULL minDist) (NULL minEtat)) (< (distance2 etat e) minDist))
	       (> (list-length (successeurs e)) 1)
	       (NOT (MYMEMBER e parcouru)))
	  (progn
	    (setq minDist (distance etat e))
	    (setq minEtat e)
	    )
	)
      )
    ;;;;(format t "~%~t~t~tON CHOISI ~a~%" minEtat)
    minEtat
    )
  )
