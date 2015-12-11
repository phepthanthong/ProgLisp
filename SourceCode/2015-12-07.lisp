(defun carre (x)
  (* x x))

(defun cube (x)
  (* x x x))

;; (defun doubler (x)
;;   (* 2 x))

(defun my-gcd (x y)
  "pgcd de 2 entiers"
  (if (= x y)
      x
      (if (> x y)
	  (my-gcd (- x y) y)
	  (my-gcd x (- y x)))))

(length '(1 2 3))
