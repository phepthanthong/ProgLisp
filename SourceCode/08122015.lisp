#|| 
- quote: faire READ sans EVALUER
- CL-USER: un paquetage dans lequel je suis. 
- en lisp, tout est pointeur, donc on ne le voit pas
- un symbole occupe un seul endroit dans la memoire. Comparer 2 symboles c'est comparer 2 pointeurs. Utiliser operateur "eq"

* Objet de premiere classe
en C: 
  - short, double
  - int, float
  - char
  - struct
  - pointeur

 (mapcar f (e1 ... en)) => retourner une liste (f(e1) ... f(en))

 (function nomDeFonction) = #'nomDeFonction

sort est une fonction destructive = mauvaise pour la prog fonctionnelle

utiliser Let: efficacite et visibilite

 () = '() = NIL 
 (cons e list): ajout de e tete de la liste
 (append l (list e)): ajout en fin de liste
 (cons '() '(1)) => (NIL 1)
 (list '() '(1)) => (NIL (1))
 (append '() '(1)) => (1)
 (cons '() 1) => (NIL . 1)
 (( 1 2 . 3) 5 6 . 7)


 (adjoin e ensemble) => sans doublon d'element
||#

(defun rouge (n)
  (+ (* n 3) 5))

(defun composition (f g)
  (lambda (x)
    (funcall g (funcall f x))))

(defun f (n)
  (let ((v1 (sqrt n)) 
	(v2 (log n))) ; let: affecter les variables en meme temps
    (* (+ v1 v2) (- v1 v2))))

(defun f1 (n)
  (let* ((v1 (sqrt n)) 
	 (v2 (log v1))) ; let*: affecter en sequentiel les variables 
    (* (+ v1 v2) (- v1 v2))))

#||
tester si une liste est vide: (endp l)
||#
(defun greater-than (l x)
  "list of elements of l which are greater than x"
  (if (endp l)
      '() ; si l est vide, retourne une liste vide
      (if (> (car l) x)
	  (cons (car l) (greater-than (cdr l) x))
	  (greater-than (cdr l) x))))

(defun greater-than1 (l x)
  "list of elements of l which are greater than x"
  (if (endp l)
      '() ; si l est vide, retourne une liste vide
      (let ((e (car l))
	    (ll (greater-than (cdr l) x)))
	(if (> (car l) x)
	    (cons e ll)
	    ll ))))

(defun my-length (l)
  (if (endp l)
      0
      (1+ (my-length (cdr l)))))

(defun riota (n)
  (if (zerop n)
      '()
      (cons n (riota (1- n)))))

(defun fact-naive (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))

(defun fact-aux (n p)
  (if (zerop n)
      p
      (fact-aux (1- n) (* n p))))

(defun fact (n)
  (fact-aux n 1))

(defun my-length-aux (l s)
  (if (endp l)
      s
      (my-length-aux (cdr l) (+1 s))))

(defun my-newlength (l)
  (labels ((aux (l s)
	     (if (endp l)
		 s
		 (aux (cdr l) (+1 s))))
	   (aux l 0))))

(defun iota (n)
  (reverse (riota n)))
(defun iota-q (n) ;; (append l (list e))
  (if (zerop n)
      '()
      (append (iota (1- n)) (list n))))
  
