;;; EXERCICE 1 ;;;
(defun valeur-en-un (f)
  (funcall f 1))

(defun polynome (x) (+ (* 3 x x) 4.7))

(valeur-en-un #'polynome)

;;; EXERCICE 2 ;;;
(defun sigma (f n p) 
  (if (> n p)
      0
      (+ (funcall f n)
	 (sigma f (1+ n) p))))

(defun sqr (x)
  (* x x))

;; (sigma (function sqr) 1 10)
;; (sigma #'sqr 1 10)
;; (sigma (lambda (x) (/ (1+ (sqrt x)))) 1 10)

;;;; EXERCICE 3 ;;;;;;;;;;;;;;;;;;;;;;;      
(defun multiplicateur (n)
  (lambda (x) (* n x)))

;; (multiplicateur 3)
;; (funcall (multiplicateur 3) 5)
;; (setf (symbol-function 'doublebis) (multiplicateur 2))
;; (function doublebis)
;; (doublebis 5)

;;;; EXERCICE 4 ;;;
(defun derivee-approx (f precision)
  (lambda (x)
    (/ (- (funcall f (+ x precision))
	  (funcall f (- x precision)))
       (* 2 precision))))

(derivee-approx #'sin 0.001d0)
(derivee-approx #'polynome 0.001d0)

(funcall (derivee-approx #'sin 0.001d0) 1)
(funcall (derivee-approx #'polynome 0.0001d0) 1)

(defun derivee-n-approx (f ordre precision)
  (if (zerop ordre)
      f
      (derivee-n-approx (derivee-approx f precision) (1- ordre) precision)))

;; (funcall (derivee-n-approx #'polynome 0 1d-5) 1)
;; (funcall (derivee-n-approx #'polynome 1 1d-5) 1)
;; (funcall (derivee-n-approx #'polynome 2 1d-5) 1)

;;; Exercice 4.3 ;;;
(defun derivee-n-approx2 (f ordre h)
  (case ordre
    (0 f)
    (1 (derivee-approx f h))
    (2 (lambda (x)
	 (/ (+ (funcall f (+ x h))
	       (* -2 (funcall f x))
	       (funcall f (- x h)))
	    (* h h))))
    (3 (let ((h*2 (* 2 h)))
	 (lambda (x)
	   (/ (+ (- (funcall f (- x h*2)))
		 (* 2 (funcall f (- x h)))
		 (* -2 (funcall f (+ x h)))
		 (funcall f (+ x h*2)))
	      (* 2 h h h)))))
    (otherwise
     (derivee-n-approx f ordre h))))

;;; EXERCICE 5 ;;;
;; version avec associativité à droite
(defun op-prod (f operation n p)
  (if (> n p)
      (funcall operation)
      (funcall operation
	       (funcall f p)
	       (op-prod f operation n (1- p)))))

(defun fact (n)
  (op-prod #'identity #'* 1 n))

(fact 6)

;;; approximation du nombre e par n itérations
(defun e-approx (n)
  (op-prod (lambda (n) (/ (fact n)))
	   #'+
	   0
	   n))
	     
(float (e-approx 10))
;;; remarque: pas efficace

;;; (plus efficace!)
(defun serie-aux (f x n i terme)
  "somme des f(i)x^i/i! jusqu'à n"
  (if (> i n)
      0
      (+ (* (funcall f i) terme)
	 (serie-aux f x n (1+ i) (/ (* terme x) (1+ i))))))

(defun serie (f x n)
  (serie-aux f x n 0 1))
       
;;; série exponentielle
(serie (lambda (x) (declare (ignore x)) 1) 1 3)

;;; cosinus : 1 - x^2! + x^4/4! - x^6/6! ...
(defun cos-approx (x nb-pas)
  (serie (lambda (i)
	   (cond ((oddp i) 0)
		 ((oddp (/ i 2)) -1)
		 (t 1)))
	 x
	 nb-pas))

(cos-approx (/ pi 4) 100)

;;;;;; EXERCICE 6 ;;;;;;;;;;;;;;;;;;
(defun produit (f g n)
  (sigma (lambda (i)
	   (* (funcall f i)
	      (funcall g (- n i))))
	 0 n))

(produit #'identity #'identity 5)

(defun convolution (f g)
  (lambda (n) (produit f g n)))

;;; autre solution pour produit
(defun produit-aux (f g i j)
  "somme de k = i à j de f(k)g(j-k)"
  (if (> i j)
      0
      (+ (* (funcall f i)
	    (funcall g (- j i)))
	 (produit-aux f g (1+ i) j))))

(defun produit-bis (f g n)
  "somme de k = 0 à n de f(k)g(j-k)"
  (produit-aux f g 0 n))
