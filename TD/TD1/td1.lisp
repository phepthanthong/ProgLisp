;; Ex1.3
(defun maximum ()
  "Retourner le maximum"
  (max 1 5 47 8 6 2))

;; Ex1.5
(defun addition (x)
  (+ x 3))

(defun carre (x)
  (* x x))

(defun valAbsolue (x)
  (if (< x 0)
      (- x)
      x))

(defun moyenne (a b)
  (/ (+ a b) 2))

(defun somme (n p)
  "Calculer la somme de n a p"
  (if (= p n)
      p
      (+ n (somme (1+ n) p))))

(defun somme1 (n p)

  )

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun fibo (n)
  (if (<= n 2)
      1
      (+ (fibo (1- n)) (fibo (- n 2)))))

