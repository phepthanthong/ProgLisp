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

    
