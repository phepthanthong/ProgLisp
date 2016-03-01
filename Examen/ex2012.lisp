;; Exercice 3
(defun sigma (f n p)

)

(defun fact

)
(defun serie (f n)
  (if (zerop n)
      (funcall f 1)
      (+ (* (expt (- 1) n)
	    (funcall f (1+ (* 2 n))))
	 (serie f (1- n)))))

(defun mystere2 (l f)
  (labels ((aux (l lres)
	     (if (endp l)
		 lres
		 (aux (cdr l)
		      (if (funcall f (car l))
			  (append lres (list (car l)))
			  lres)))))
    (aux l '())))

(defun mystere3 (l f)
  (labels ((aux (l lres)
	     (if (endp l)
		 (nreverse lres)
		 (aux (cdr l)
		      (if (funcall f (car l))
			  (cons (car l) lres)
			  lres)))))
    (aux l '())))


;; 2013
(defun expand (v)
  (if (endp v)
      nil
      (dotimes (i (length v))
	(cons (make-list (aref v))

(defun print-image (img)
  (if (


;; 2015
(defun compte-occ (atom l) 
    (if (endp l)
	0
	(progn 
	  (if (eq atom (car l))
	      (1+ (compte-occ atom (cdr l)))
	      (compte-occ atom (cdr l)))
	  (if (not (atom (car l)))
	      (+ (compte-occ atom (car l))
		 (compte-occ atom (cdr l)))
	      (compte-occ atom (cdr l))))))

(defun compose (f g)
  (lambda (x)
    (funcall g (funcall f x))))
    


