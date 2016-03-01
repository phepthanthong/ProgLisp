;;; avant de compiler chager la bibliotheque "imago"
;;; CL-USER> (ql:quickload "imago")
;;; pour simplifier travailler dans le package :zones
;;; CL-USER> (in-package :zones)  (ou raccourci C-c M-p + nom package)
;;; ZONES> 
;; pour sauver une zone dans un fichier
;;  ZONES> (zone-to-png  (zone-difference (make-disk 30) (make-disk 20)) 50 "anneau.png")
;;; le fichier sauve se trouve dans le repertoire courant reperable
;;; grace a la variable *default-pathname-defaults*
;;; ZONES> *default-pathname-defaults
;;; #P"/nfs4/home4/idurand/Enseignement/PFS/TD-PFS/Zones/"
;;; visualiser l'image avec le viewer de votre choix

(defpackage #:zones (:use #:common-lisp #:imago))

(in-package :zones)

;;; visualisation
(defvar *nb-pixels-per-unit* 10)

(defun int-to-nb-pixels (n)
  (* *nb-pixels-per-unit* n))

(defun pixel-to-float (x)
  (/ x *nb-pixels-per-unit*))

(defun pixel-to-point (px py)
  (complex
   (pixel-to-float px)
   (pixel-to-float py)))

(defun zone-to-pixels (zone n)
  (loop
    with nb-pixels = (int-to-nb-pixels n)
    with pixels = (make-array
		   (list nb-pixels nb-pixels)
		   :element-type 'rgb-pixel
		   :initial-element +black+)
    for x from 0 below nb-pixels
    do (loop
	 for y from 0 below nb-pixels
	 do (setf (aref pixels x y) (if (point-in-zone-p (pixel-to-point x y) zone) +red+ +white+)))
    finally (return (coerce pixels '(simple-array rgb-pixel (* *))))))

(defun pixels-to-png (pixels filename)
  (write-png (make-instance 'rgb-image :pixels pixels) filename))
    
(defun zone-to-png (zone n filename)
  (pixels-to-png (zone-to-pixels zone n) filename))

(defun random-point (z)
  (complex (random z) (random z)))
  
(defun gruyere (n)
  (loop
    with zone = (make-disk 40)
    with disk = (make-disk 5)
    repeat n
    do (setq zone (zone-difference zone (move-zone disk (random-point 40))))
    finally (return zone)))

 ;; (zone-to-png (gruyere 5) 50 "gruyere.png")

;;; ==============================================================
;;; A zone is represented as a function that takes a point in 2-dimensional 
;;; space as a parameter (represented as a complex  number), and returns T 
;;; if and only if the point is in the zone, and NIL otherwise.

;;; To determine whether a point is in a zone, just call this function
(defun point-in-zone-p (point zone)
  (funcall zone point))

;;; A zone that contains no points.  A point is never in this zone. 
(defparameter +nowhere+
  (lambda (p)
    (declare (ignore p))
    nil))

;;; A zone that constains all points
(defparameter +everywhere+
  (lambda (p)
    (declare (ignore p))
    t))

;;; Create a circular zone with center in (0,0) with the indicated radius.
(defun make-disk (radius)
  (assert (realp radius))
  (lambda (p)
    (<= (abs p) radius)))

;;; Given two zones, create a zone that behaves as the intersection of the two.
(defun zone-intersection (zone1 zone2)
  (lambda (p)
    (and (point-in-zone-p p zone1)
	 (point-in-zone-p p zone2))))

;;; Given two zones, create a one that behaves as the union of the two
(defun zone-union (zone1 zone2)
  (lambda (p)
    (or (point-in-zone-p p zone1)
	(point-in-zone-p p zone2))))

(zone-to-png (zone-union (make-disk 20) (make-disk 10)) 50 "test-png.png")

;;; Given a zone, move it by a vector indicated as a complex number
;;; passed as the argument.
(defun move-zone (zone vector)
  (lambda (p)
    (point-in-zone-p (- p vector) zone)))

;;; Given two zones, create a zone that behaves as the difference of the two. 
(defun zone-difference (zone1 zone2)
  (lambda (p)
    (and (point-in-zone-p p zone1)
	 (not (point-in-zone-p p zone2)))))

;;; Construire une zone rectangulaire de largeur et de hauteur dont le sommet inferieure gauche est (0,0)
(defun make-rectangle (width height)
  (lambda (p) (and 
	       (and (<= 0 (realpart p))
		    (<= (realpart p) width))
	       (and (<= 0 (imagpart p))
		    (<= (imagpart p) height)))))

;;; La fonction scale-zone applique a la zone la transformation (x,y) => (a*x,b*y)
;;; avec a et b la partie reelle et celle imaginaire d'un nombre complexe
(defun scale-zone (zone nombre)
  (assert (and (not (= 0 (realpart nombre))) (not (= 0 (imagpart nombre)))))
  (let ((preel (realpart nombre))
	(ipart (imagpart nombre)))
    (lambda (p)
      (point-in-zone-p (complex (* p preel) (* p ipart)) zone))))

(zone-to-png (scale-zone (zone-difference (make-disk 20) (make-disk 10)) #C(1 4) ) 50 "test-png.png")

(defun make-disk-bis (radius &optional (centre #C(0 0)))
  (lambda (p)
    (< (abs (-p centre)) radius)))
;;; La fonction rotate-zone dont le but est la rotation d'une zone autour d'une origine
(defun rotate-zone (zone angle)
  (lambda (p)
    (let ((real (realpart p))
	  (imag (imagpart p)))
      (point-in-zone-p (complex (- (* real (cos angle)) (* imag (sin angle)))
				(+ (* imag (cos angle)) (* real (sin angle))))
		       zone))))

(defun test-rotate-zone()
  (assert (point-in-zone-p #C(1 1) (rotate-zone (make-rectangle 3 2) (/ pi 2)))))

(defun scale-zone1 (zone nombre &optional (centre #C(0 0)))
  (move-zone 
   (scale-zone
    (move-zone zone (- centre))) centre)) 
    
;; 13
(defun rotate-zone1 (zone nombre &optional (centre #C(0 0)))
  (move-zone
   (rotate-zone
    (move-zone zone centre) nombre) (- centre)))
(defun test-rotate-zone1()
  (assert (point-in-point-p #C(0 1) (rotate-zone1 (make-disk 1) (/ pi 2)))))

;; 17
(defun determinant (x y)
  (- (* (realpart x) (imagpart y))
     (* (realpart y) (imagpart x))))

(defun make-half-plane(p1 p2)
  (lambda (p) (<= (determinant (- p2 p1) (- p p1)) 0)))

;; 18
(defun make-triangle (p1 p2 p3)
  (if (point-in-zone-p p3 (make-half-plane p1 p2))
  (zone-intersection 
   (zone-intersection (make-half-plane p1 p2)
		      (make-half-plane p2 p3))
   (make-half-plane p3 p1))
  (zone-intersection 
   (zone-intersection (make-half-plane p1 p3)
		      (make-half-plane p3 p2))
   (make-half-plane p2 p1))))

(defun test-triangle ()
  (assert (null (ignore-errors (point-in-zone-p #c(0 0)

;; 19
(defun det-lon-hon-0 (n1 n2)
  (>= (determinant n1 n2) 0))
(defun make-sector (nb1 nb2)
  (let ((e1 (exp (complex 0 nb1)))
	
