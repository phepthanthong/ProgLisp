;;;;;;;;;; Exercice 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((compteur 0)) ;; variable locale permanente
  (defun ticket ()
    (incf compteur))
  (defun raz ()
    (setf compteur 0)))

;;;;;;;;;; Exercice 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *g* 9.81) 

;;; ne pas appeler une fonction 't' qui est réservé pour le booléen vrai!
(defun periode (len) 
  (* 2 pi (sqrt (/ len *g*))))

;; (periode 3)
;; (setf *g* 1.62)
;; (periode 3)

;; CL-USER> (defconstant +truc+ 3) ==> +TRUC+
;; CL-USER> (setf +truc+ 2)

;; In: SETF +TRUC+
;;   (SETF +TRUC+ 2)
;; ==>
;;   (SETQ +TRUC+ 2)
;; Error: Attempt to set constant +TRUC+.

;;;;;;;;;; Exercice 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun derivee-approx (f &optional (precision 1d-5))
  (lambda (x)
    (/ (- (funcall f (+ x precision))
	  (funcall f (- x precision)))
       (* 2 precision))))

(defun derivee-n-approx (f ordre &optional (precision 1d-5))
  (do ((i ordre (1- i))
       (d f (derivee-approx d precision)))
      ((zerop i) d)))

(defun op-prod (f operation element-neutre n p)
  (do ((p p (1- p))
       (acc element-neutre
            (funcall operation
                     (funcall f p)
                     acc)))
      ((> n p) acc)))

(defun serie (f x n)
  (do* ((i 0 (1+ i))
        (terme 1 (/ (* terme  x) i))
        (acc (funcall f 0) (+ (* (funcall f i) terme) acc)))
       ((>= i n) acc)))

;;;;;;;;;; Exercice 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1-
(defun iota (n)
  (let ((l '()))
    (dotimes (i n (nreverse l))
      (push i l)))))

;; 2-
(defun iota (n &optional (start 0))
  (let ((l '())
	(e (+ n start)))
    (dotimes (i n l)
      (push (decf e) l))))

;; 3-
(defun iota (n &optional (start 0) (step 1))
  (let ((l '())
	(e (+ (* n step) start)))
    (dotimes (i n l)
      (push (decf e step) l))))

;;;;;;;;;; Exercice 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun triangle (n)
  "calcule le triangle de Pascal de hauteur n"
  (do ((tr (list '(1)) (cons (next-line (car tr)) tr))
       (i 0 (1+ i)))
       ((>= i n)(nreverse tr))))

(defun triangle (n) ; avec dotimes
  "calcule le triangle de Pascal de hauteur n"
  (let ((l '(1))
	(tr '()))
    (dotimes (i n (nreverse tr))
      (push l tr)
      (setf l (next-line l)))))

;;;;;;;;;; Exercice 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-randomize-list (n)
  (dotimes (i n)
    (test-randomize-list-once
     (random 100)
     (1+ (random 100)))))

;;;;;;;;;; Exercice 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-mapcar-fill (n)
  (dotimes (i n)
    (let* ((length (random 100))
	   (element (1+ (random 100)))
	   (list1 (random-list length element))
	   (list2 (random-list length element)))
      (test-mapcar-fill-against-mapcar #'+ list1 list2))))

;;;;;;;;;; Pour info versions avec la macro loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun derivee-n-approx (f ordre  &optional (precision 1d-5))
  (loop repeat ordre
	and d = f then (derivee-approx d precision)
	finally (return d)))

(defun op-prod (f operation element-neutre n p)
  (loop for i from n to p 
	and res = element-neutre then (funcall operation res (funcall f i))
	finally (return res)))
;; quand "i" prend à la valeur "p + 1", "res" recoit "res + f(i)" avec la 
;; valeur précédente de "i" qui est "p"

(defun serie (f x n)
  (loop for i from 0 to n
	for aux = 1 then (/ (* aux x) i)
	for terme = (funcall f 0) then (* (funcall f i) aux)
	sum terme))

(defun iota (n)
  (loop for i from 0 below n
	collect i))

(defun iota (n &optional (start 0))
  (loop repeat n
	for i from start
	collect i))

(defun iota (n &optional (start 0) (step 1))
  (loop repeat n
	for i from start by step
	collect i))

(defun triangle (n)
  (loop	repeat n
	for l = '(1) then (next-line l)
	collect l))

(defun test-randomize-list (n)
  (loop repeat n
	do (test-randomize-list-once
	    (random 100)
	    (1+ (random 100)))))

(defun test-mapcar-fill (n)
  (loop repeat n
	do (let* ((length (random 100))
		  (element (1+ (random 100)))
		  (list1 (random-list length element))
		  (list2 (random-list length element)))
	     (test-mapcar-fill-against-mapcar #'+ list1 list2))))
