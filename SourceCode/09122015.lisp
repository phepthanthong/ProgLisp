#||

 (acons key value alist) = (cons (cons key value) alist)

*Egalite

eq : comparer si les 2 objets sont dans un meme endroit (comparer les addresses)
     pour les gros nombres, ne marche pas

#C(partieReelle partieImaginaire) : nombre complexe
#( : un tableau
# derriere est un type constante
#\ : un caractere

**Appel de fonction
 (dotimes (i n)
    e1
    en) = for(int i = 0; i< n; i++)

 (format flot "~4D ~42F ~%" 30)
         *standard-output*
         t
 => format forme les expressions tres compliquees

 pop: fait un effet de bord (changer la liste initiale)
 cdr: ne fait pas d'effet de bord
 (setq l (cdr l)) = (pop l)

 (push e l) = (setq l (cons e l))
||#
(defun f (l)
  (dotimes (i (length l))
    (format t "i = ~A : ~A~%" i (car l))
    (pop l)))

(defun g (a b &optional c (d 2))
  (list a b c d))
(defun h (a &rest l)
  (list a l))

(defun k (&key couleur image largeur hauteur)
  (list couleur image largeur hauteur))

(defun fact (n)
  (assert (integerp n))
  (assert (>= n 0))
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun zeros (n)
  (make-list n :initial-element 0))

(defun test-zero (n)
  (let ((l (zero n)))
    (assert (listp l))
    (assert (= (length l) n))
    (assert (every #'integerp l))
    (assert (every #'zerop l ))))

(defun leaves (tree)
  (if (atom tree)
      (list tree)
      (append (leaves (car tree)) (leaves (cdr tree)))))

;; (progn e1 ... en)
(defun leaves2 (tree)
  (let ((l '()))
    (labels ((aux (tree)
	       (if (atom tree)
		   (push tree l)
		   (progn (aux (car tree)) (aux (cdr tree))))))
      (aux tree)
      l)))
