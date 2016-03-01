;;;;;;;;;; Exercice 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-histogram (nmax)
  (make-array (1+ nmax) :initial-element 0))

(defun histogram-bound (h)
  (length h))

(defun histogram-total (h)
  (reduce #'+ h))

(defun histogram-add (h k)
  (assert (and (<= 0  k) (< k (histogram-bound h))))
  (incf (aref h (floor k))))

(defun histogram-fill-random (h m)
  (let ((n (histogram-bound h)))
    (dotimes (i m h)
      (histogram-add h (random n)))))

(defun random-histogram (nmax m)
  (histogram-fill-random (make-histogram nmax) m))

(defun histogram-enter-notes (h notes)
   (let ((n (histogram-bound h)))
     (dotimes (i (length notes) h)
       (let ((note (aref notes i)))
	(assert (<= 0 note (1- n)))
	(histogram-add h (floor (aref notes i)))))))

(defun histogram-from-notes (n notes)
  (histogram-enter-notes (make-histogram n) notes))

(defun histogram-reset (h)
  (dotimes (i (histogram-bound h) h)
    (setf (aref h i) 0)))

(defun print-nstars (n stream)
  (format stream (make-string n :initial-element #\*)))

;; (defun print-hist-line (k stream)
;;   (format stream "[~3D] " k)
;;   (print-nstars k stream)
;;   (format stream "~%"))

;; (defun histogram-print (h &optional (stream t))
;;   (dotimes (i (length h))
;;     (format stream "~2D " i)
;;     (print-hist-line (aref h i) stream)))

;; avec fonction d'impression de ligne definie localement
(defun histogram-print (h &optional (stream t))
  (flet ((print-line (k)
	   (format stream "[~3D] " k)
	   (print-nstars k stream)
	   (format stream "~%")))
    (format stream "~%")
    (dotimes (i (length h))
      (format stream "~2D " i)
      (print-line (aref h i)))))

(defparameter *m* 200)
(defparameter *n* 11)
(defparameter *histogram* (make-histogram *n*))
(histogram-fill-random *histogram* *m*)
(histogram-print *histogram*)

(defparameter *flot* (open "HISTOGRAMME" :direction :output))
(print "Histogramme" *flot*)
(terpri *flot*)
(histogram-print *histogram* *flot*)
(close *flot*)

(defun print-nstars0 (n)
  (format *standard-output* (make-string n :initial-element #\*)))

(defun histogram-print0 (h)
  (flet ((print-line (k)
	   (format *standard-output* "[~3D] " k)
	   (print-nstars0 k)
	   (format *standard-output*"~%")))
    (dotimes (i (length h))
      (format *standard-output* "~2D " i)
      (print-line (aref h i)))))

(histogram-print0 *histogram*) ;;; sur la sortie standard

(let ((*standard-output* (open "HISTOGRAMME" :direction :output :if-exists :supersede)))
  (format t "~A~%~%" "Histogramme 2008-2009")
  (histogram-print0 *histogram*) ;;; sur le flot ouvert sur le fichier
  (format t "~%FIN~%")
  (close *standard-output*))

(histogram-print0 *histogram*) ;;; sur la sortie standard

;;;;;;;;;; Exercice 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tables de hachage
(defclass color ()
  ((r :initarg :r :reader red-of)
   (g :initarg :g :reader green-of)
   (b :initarg :b :reader blue-of)))

(defmethod print-object ((c color) stream)
  (format stream "couleur{red=~d, green=~d, blue=~d}"
	  (red-of c)
	  (green-of c)
	  (blue-of c)))

(defun make-color (&key (r 0) (g 0) (b 0))
  (make-instance 'color :r r :g g :b b))

;;;;;;;;;; 2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun warm-colorp (color)
  (> (red-of color) (blue-of color)))

(warm-colorp (make-color :r 255 :g 128 :b 123))
(warm-colorp (make-color :r 155 :g 128 :b 183))

;;;;;;;;;; 2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *colors* (make-hash-table :test #'eq))
(describe *colors*)   
(setf (gethash 'red *colors*) (make-color :r 255))
(setf (gethash 'green *colors*) (make-color :g 255))
(setf (gethash 'blue *colors*)  (make-color :b 255))
(setf (gethash 'white *colors*) (make-color :r 255 :g 255 :b 255))
(setf (gethash 'black *colors*) (make-color))
(setf (gethash 'gray *colors*) (make-color :r 128 :g 128 :b 128))
(setf (gethash 'orange *colors*) (make-color :r 255 :g 128))
(setf (gethash 'yellow *colors*) (make-color :r 255 :g 255))
(describe *colors*)
(describe (gethash 'blue *colors*))

;;;;;;;;;; 2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun keylist (table)
  (let ((l '()))
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k l))
	     table)
    l))

(keylist *colors*) ;(YELLOW ORANGE GRAY BLACK WHITE BLUE GREEN RED) 

;;;;;;;;;; 2.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-hash-table-iterator 
    (get-color *colors*) (get-color))
;;T
;;RED
;;couleur{red=255, green=0, blue=0}     

(with-hash-table-iterator 
    (get-color *colors*) (get-color))

(with-hash-table-iterator 
    (get-color *colors*) (get-color) (get-color)) 
;;T
;;GREEN
;;couleur{red=0, green=255, blue=0}   

;;;;;;;;;; 2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-warm-colors (colors &optional stream) 
  (with-hash-table-iterator (get-color colors)
    (do () (nil)
      (multiple-value-bind (got-one k v) (get-color)
	(if got-one
	    (when (warm-colorp v)
	      (print k stream))
	    (return))))))

(print-warm-colors *colors*)
;; YELLOW
;; RED
;; ORANGE
;; NIL

;; autre solution (pour s'entraîner à utiliser 'labels': 
;; la boucle est simulée par un appel récursif
(defun print-warm-colors-bis (colors &optional stream) 
  (with-hash-table-iterator (get-color colors)
    (labels ((f ()
	       (multiple-value-bind (got-one k v) (get-color)
		 (when got-one
		   (when (warm-colorp  v)
		     (print k stream))
		   (f)))))
      (f))))

;; Construit la liste des couleurs chaudes
(defun list-warm-colors (colors) 
  (with-hash-table-iterator (get-color colors)
    (let ((l ()))
      (do () (nil)
	(multiple-value-bind (got-one k v) (get-color)
	  (if got-one
	      (when (warm-colorp v)
		(push k l))
	      (return l)))))))

(list-warm-colors *colors*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Histogramme avec des objets et sans variables globales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defclass histogram ()
;;   ((bound :initarg :bound :reader histogram-bound)
;;    (total :initform 0 :accessor histogram-total)
;;    (values :accessor histogram-values)))

;; (defun make-histogram-bis (bound)
;;   (let ((h (make-instance 'histogram :bound bound)))
;;     (setf (histogram-values h) 
;; 	  (make-array bound :initial-element 0))
;;     h))

;; (defun histogram-add (h k)
;;   (incf (aref (histogram-values h) k))
;;   (incf (histogram-total h))
;;   h)

;; (defun print-histogram (histo &optional (stream t))
;;   (dotimes (i (histogram-bound histo))
;;     (format stream "~2D " i)
;;     (print-hist-line (aref (histogram-values histo) i) stream))
;;   (format stream "~%Total ~d" (histogram-total histo)))

;; (defparameter *histogram* (make-histogram-bis 20))

;; (defun histogram-fill-random (h m)
;;   (let ((n (histogram-bound h)))
;;     (dotimes (i m h)
;;       (histogram-add h (random n)))))

;; (histogram-fill-random *histogram* 200)
;; (print-histogram *histogram*)
