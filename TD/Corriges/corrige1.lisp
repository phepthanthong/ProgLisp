;;; Exercice 1.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CL-USER> 3

3
CL-USER> 3.2

3.2
CL-USER> 3/4

3/4
CL-USER> #C(1 4)

#C(1 4)
CL-USER> #(1 2 3 4)

#(1 2 3 4)
CL-USER> #\a

#\a
CL-USER> "cheval"

"cheval"

CL-USER> nil

NIL
CL-USER> t

T

;;; Exercice 1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CL-USER> (+ 3 4)

7
CL-USER> (* 3.2 4.5)

14.400001
CL-USER> (/ (abs -5.3) (cos 4.3))

-13.223586
CL-USER> (- 3/4 4/5)

-1/20
CL-USER> (* #C(1 3) #(2 4))

CL-USER> (* #C(1 3) #C(2 4))

#C(-10 10)

;;; Exercice 1.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CL-USER> (+ 1 2 3 4)

10
CL-USER> (max 1 2 3 4 5 6 6) 

6
CL-USER> (log 4 3)

1.2618595
CL-USER> (log 4)

1.3862944

;;; Exercice 1.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CL-USER> (integerp 3)

T
CL-USER> (integerp 3.1)

NIL
CL-USER> (realp 3)

T
CL-USER> (realp 3.1)

T
CL-USER> (stringp "cheval")

T
CL-USER> (complexp #C(1 2))

T
CL-USER> (characterp #\a)

T
CL-USER> (typep 23.32 'real)
T

CL-USER> (typep 23 'integer)
T

;;; Exercice 1.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun f (x)
  (+ x 3))

(defun sqr (x)
  (* x x))

(defun my-abs (x)
  (if (plusp x)
      x
      (- x)))

(defun moyenne (a b)
  (/ (+ a b) 2.0))

;version avec un nombre arbitraire de valeurs
(defun moyenne (&rest valeurs)
  (/ (reduce #'+ valeurs) (length valeurs)))

(defun sigma (n p)
  (if (> n p)
      0
      (+ n (sigma (1+ n) p))))

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))
 
;; remarque: ce ne sont pas les versions efficaces
;; qui elles sont itératives ou récursives terminales

(defun fib (n)
  (if (<= n 1)
      1
      (+ (fib (1- n)) (fib (- n 2))))))

;;; Exercice 1.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fact-aux (i f)
  (if (zerop i)
      f
      (fact-aux (1- i) (* i f))))

(defun fact (n)
  (fact-aux n 1))

;;; Exercice 1.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



