;; 6.2
(defparameter *g* 9.81)

(defun calcul-periode-pendule (l)
  (* 2 pi (sqrt (/ l *g*))))

;; 6.5.4
(defun next-line (l)
  (mapcar #'+ (cons 0 l) (append l (list 0))))

(defun triangle-bis (n)
  (let ((l '(1))
	(tmp '((1))))
    (dotimes (i n)
      (setq tmp (cons (setf l (next-line l)) tmp)))
    (nreverse tmp)))

(defun triangle-bis1 (n)
  (let ((l '(1))
	(l-vide '()))
    (dotimes (i n (nreverse l-vide))
      (push l l-vide)
      (setf l (next-line l)))))

(triangle-bis 4)
(triangle-bis1 4)

;; 6.5.5
(defun op-prod (f operation n p &optional (key 0))
  (let ((x key))
    (loop
	 for i from n to p
	 do (setq x (funcall operation (funcall f i) x)))
    x)) 

(op-prod (lambda (x) (1+ x)) #'* 1 3 1)

(defun serie-aux (f x i)
  (/ (* (funcall f i)
	(expt x i))
     (op-prod (lambda (k) k) #'* 1 i 1)))

(defun serie (f p x)
  (let ((result 0))
    (dotimes (i (+ p 1))
      (setq result (+ result (serie-aux f x i))))
    result))
 



