;; 2.1
(defun f (x)
  (funcall x 1))

(defun fx (x)
  (+ (* 3 (* x x)) 4.7))

;; 2.2
(defun cong-mot (x)
  (+ x 1))

(defun sigma (f n p)
  (if (> n p)
      0
      (+ (funcall f n) (sigma f (1+ n) p))))

;; 2.3
(defun mul (n)
  (lambda (x) (* x n)))

(defun double-arg ()
  (setf (symbol-function 'doubler) (mul 2)))

;; 2.4
(defun f-test (x)
  (* x x))

(defun derivee (f h)
  (lambda (x) (/ (- (funcall f (+ x h)) (funcall f x)) h)))

(funcall (derivee #'f-test 0.1) 1)

(defun derivee-nieme (f h n)
  (if (zerop n)
      f
      (derivee-nieme (derivee f h) (1- n) h)))

(defun fx4 (x)
  (* x x x x))

(funcall (derivee-nieme #'fx4 5 1) 1) 

(defun derivee-a2 (f h)
  (lambda (x)
    (/ (+ (funcall f (+ x h) (* -2 (funcall f x)) (funcall f (- x h)))) (* h h))))

(defun derivee-a3 (f h)
  (lambda (x)
    (+ (- (funcall f (- x (* 2 h)))) (* 2 (funcall f (- x h))) (- (* 2 (funcall f (+ x h)))) (funcall f (+ x (* 2 h))))))
    

(defun derivee-new (f h n)
  (lambda (x)
    (cond ((= n 0) f)
	  ((= n 1) (derivee f h))
	  ((= n 2) (derivee-a2 f h))
	  ((= n 3) (derivee-a3 f h))
	  (t (derivee-nieme f h n)))))

;; Exercice 2.5
;; 2.5.1
(defun op (f n p operation)
  (if (> n p)
      (funcall operation)
      (funcall operation (funcall f n) (op f (+ n 1) p operation)))) 
(op #'f 1 3 '+)

;;2.5.2
(defun fact (n)
  (op (lambda (x) x) 1 n #'*))
(fact 4)

;; 2.5.3
(defun approx (n)
  (+ 1 (op (lambda (x) (/ (fact x))) 1 n #'+)))

(approx 2)

;; 2.5.4
(defun somme (f x p)
 (op (lambda (i) (/ (* (funcall f i) (expt x i)) (fact i)))
     1
     p
     #'+))

(somme #'cong-mot 1 3)

;; 2.5.5
(defun somme-bis (f g n x)
  (op (lambda (i) (/ (* (funcall f i) (expt x (funcall g i)))
		     (fact (funcall g i))))
      0
      n
      #'+))
      
;; Exercice 2.6
;; 2.6.1
(defun ff (x)
  (* x x))

(defun g (x)
  (* x x x))

(defun produit (f g n)
  (sigma (lambda (x) (* (funcall f x) (funcall g (- n x)))) 0 n))

(produit #'ff #'g 3)

(defun produit-fun (f g)
  (lambda (n) (produit f g n)))

(funcall (produit-fun #'ff #'g) 2)
