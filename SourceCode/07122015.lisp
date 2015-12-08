(defun carre (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun doubler (x)
  (* 2 x))

(defun pgcd (x y)
  "pgcd de 2 entiers"
  (if (= x y)
      x
      (if (> x y)
	  (pgcd(- x y) y)
	  (pgcd x (- y x)))))

#|| setf plus puissant que setq
affecter une variable => setq
||#
