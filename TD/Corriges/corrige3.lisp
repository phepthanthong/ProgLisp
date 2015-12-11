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

;;; Create a circular zone with center in (0,0) with the indicated radius.
(defun make-disk (radius)
  (lambda (p)
    (<= (abs p) radius)))

;;; Given two zones, create a zone that behaves as the intersection of the two.
(defun zone-intersection (zone1 zone2)
  (lambda (p)
    (and (point-in-zone-p p zone1)
	 (point-in-zone-p p zone2))))

;;; Given a zone, move it by a vector indicated as a complex number
;;; passed as the argument.
(defun move-zone (zone vector)
  (lambda (p)
    (point-in-zone-p (- p vector) zone)))

;;; Test all zone-manipulating code.
(defun test ()
  (let* ((c (make-disk 1))
	 (c1 (move-zone c #C(1 0))))
    (assert (point-in-zone-p #C(0.0 0.5) c))
    (assert (not (point-in-zone-p #C(1.0 0.5) c)))
    (assert (point-in-zone-p #C(0.5 0) (zone-intersection c c1)))))

;; (test)

;;; A zone that contains every point. A point is always in this zone
(defparameter +everywhere+
  (lambda (p)
    (declare (ignore p))
    t))

;; (funcall +everywhere+ (complex (random 10) (random 10)))

;;; Given two zones, create a zone that behaves as the union of the two.
(defun zone-union (zone1 zone2)
  (lambda (p)
    (or (point-in-zone-p p zone1)
	(point-in-zone-p p zone2))))

;;; Given two zones, create a zone that behaves as the difference of the two. 
(defun zone-difference (zone1 zone2)
  (lambda (p)
    (and (point-in-zone-p p zone1)
	 (not (point-in-zone-p p zone2)))))

;;; Make a rectangle in the first quadrant.
(defun make-rectangle (width height)
  (assert (and (< 0 width) (< 0 height)))
  (lambda (p)
    (and (<= 0 (realpart p) width)
	 (<= 0 (imagpart p) height))))

;;; Check that the radius is real
(defun make-disk1 (radius)
  (assert (realp radius))
  (lambda (p)
    (<= (abs p) radius)))

;;; Optional parameter
(defun make-disk2 (radius &optional (center #C(0 0)))
  (assert (realp radius))
  (lambda (p)
    (<= (abs (- center p)) radius)))

;; (point-in-zone-p #C(2 2) (make-disk 1 #C(1.5 1.5)))

;;; Scale a zone in two dimensions
(defun scale-zone0 (zone coeff)
  (assert (not (or (zerop (realpart coeff)) (zerop (imagpart coeff)))))
  (let ((rc (realpart coeff))
	(ic (imagpart coeff)))
    (lambda (p)
      (point-in-zone-p
       (complex (/ (realpart p) rc)
		(/ (imagpart p) ic))
       zone))))

;;; Test scale-zone
;; (point-in-zone-p #C(5 5) (scale-zone (make-disk 2) #C(10 10)))

(defun rotate-zone0 (zone angle)
  (let ((e (exp (complex 0 (- angle)))))
    (lambda (p)
      (point-in-zone-p
       (* p e)
       zone))))

;; (point-in-zone-p #C(0.5 8) (rotate-zone0 (make-rectangle 10 2) (/ pi 2)))

(defun rotate-zone (zone angle &optional (center #C(0 0)))
  (let ((e (exp (complex 0 (- angle)))))
    (lambda (p)
      (point-in-zone-p
       (+ center (* (- p center) e))
       zone))))

(defun scale-zone1 (zone coeff &optional (origin #C(0 0)))
  (assert (not (or (zerop (realpart coeff)) (zerop (imagpart coeff)))))
  (move-zone
   (scale-zone0
    (move-zone zone (- origin))
    coeff)
   origin))

;; (point-in-zone-p #C(6 0.5) (scale-zone (make-disk 1) #C(4 4) #C(6 4)))

;;; Test that make-disk fails for an imaginary radius
(assert (null (ignore-errors (make-disk2 #C(0 1)))))

;;; Le déterminant sera utile pour la suite
(defun determinant (v1 v2)
  "determinant of the vectors v1 v2"
  (- (* (realpart v1) (imagpart v2))
     (* (imagpart v1) (realpart v2))))

;;; Define a zone that corresponds to the half plane to the right
;;; of a line from p1 to p2. 
(defun make-half-plane (p1 p2)
  (let ((v1 (- p2 p1)))
    (lambda (p)
      (let ((v2 (- p p1)))
	(<= (determinant v1 v2) 0)))))

(defun zones-test1 ()
  (assert (point-in-zone-p #c(1 0) (make-half-plane #c(0 0) #c(1 1))))
  (assert (not (point-in-zone-p #c(0 1) (make-half-plane #c(0 0) #c(1 1)))))
  (assert (point-in-zone-p #c(0 -1) (make-half-plane #c(0 0) #c(1 1))))
  (assert (not (point-in-zone-p #c(-1 0) (make-half-plane #c(0 0) #c(1 1)))))
  (assert (point-in-zone-p #c(100 100) (make-half-plane #c(0 0) #c(1 1))))
  (assert (not (point-in-zone-p #c(100 101) (make-half-plane #c(0 0) #c(1 1)))))
  (assert (point-in-zone-p #c(100 99) (make-half-plane #c(0 0) #c(1 1))))
  (assert (not (point-in-zone-p #c(-100 -99) (make-half-plane #c(0 0) #c(1 1)))))
  (assert (point-in-zone-p #c(-100 -101) (make-half-plane #c(0 0) #c(1 1))))
  (assert (point-in-zone-p #c(0.1 0.1) (make-half-plane #c(0 1) #c(1 0))))
  )
;;; Define a triangle as an intersection of three half planes
(defun make-triangle (p1 p2 p3)
  ;; make sure the points are in clockwise order
  (let ((pp2 (if (point-in-zone-p p3 (make-half-plane p1 p2))
		 p2
		 p3))
	(pp3 (if (point-in-zone-p p3 (make-half-plane p1 p2))
		 p3
		 p2)))
    (zone-intersection (make-half-plane p1 pp2)
		       (zone-intersection (make-half-plane pp2 pp3)
					  (make-half-plane pp3 p1)))))

(defun zones-test2 ()
  (assert (point-in-zone-p #c(0.1 0.1) (make-triangle #c(0 0) #c(0 1) #c(1 0))))
  (assert (not (point-in-zone-p #c(0.1 -0.1) (make-triangle #c(0 0) #c(0 1) #c(1 0)))))
  (assert (not (point-in-zone-p #c(0.1 1) (make-triangle #c(0 0) #c(0 1) #c(1 0)))))
  (assert (not (point-in-zone-p #c(0.1 8) (make-triangle #c(0 0) #c(0 1) #c(1 0)))))
  (assert (point-in-zone-p #c(0.1 0.8) (make-triangle #c(0 0) #c(0 1) #c(1 0))))
  (assert (not (point-in-zone-p #c(-0.1 0.8) (make-triangle #c(0 0) #c(0 1) #c(1 0)))))
)
;;; Define a triangle-every orientation of the argument is ok
(defun make-triangle2 (p1 p2 p3)
  (lambda (p)
    (and (>= (* (determinant (- p2 p1) (- p p1))
		(determinant (- p2 p1) (- p3 p1))) 0)
	 (>= (* (determinant (- p3 p2) (- p p2))
		(determinant (- p3 p2) (- p1 p2))) 0)
	 (>= (* (determinant (- p1 p3) (- p p3))
		(determinant (- p1 p3) (- p2 p3))) 0))))

(defun direct (v1 v2)
  "true if vectors v1 v2 have direct orientation"
  (>= (determinant v1 v2) 0))

;; Secteur de la demi-droite d'angle angle1 vers celle d'angle angle2.
(defun make-sector2 (angle1 angle2)
  (let ((e1 (exp (complex 0 angle1)))
        (e2 (exp (complex 0 angle2))))
    (lambda (p)
      (or  (and (direct e1 e2) (direct e1 p) (direct p e2))
	   (and (direct e2 e1) (direct e1 p))
	   (and (direct e2 e1) (direct p e2))))))

(defparameter *S4* (make-sector2 0 (/ pi 4)))

;;CL-USER> (point-in-zone-p #C(0.25 0.2) *S4*)
;;T
;;CL-USER> (point-in-zone-p #C(0.25 0.3) *S4*)
;;NIL

(defun make-sector3 (angle1 angle2 &optional (origin #C(0 0)))
  (move-zone (make-sector2 angle1 angle2) origin))

(defun make-sector4 (angle1 angle2 &optional (origin #C(0 0)) (radius nil))
  (if radius
      (move-zone (zone-intersection
		  (make-sector2 angle1 angle2)
		  (make-disk radius)) origin)
      (move-zone (make-sector2 angle1 angle2) origin)))
