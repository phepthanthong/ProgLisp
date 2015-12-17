;; 4.1

;; 4.2
;; butlast() est une fonction non-destructive 
(defun swap-first-last (l)
  "Echanger le premier et le dernier element d'une liste"
  (if (endp (cdr l))
      l
      (append (butlast (cons (car (last l))
			     (cdr l)))
	      (cons (car l) '()))))
		 
(defun swap-first-last-bis (l)
  (if (endp (cdr l))
      l
      (append (last l) (cdr (butlast l)) (list (first l)))))

(defparameter *l* '(you cant buy love))
(swap-first-last *l*)

;; 4.3
(defun rotate-left (l)
  "Une fonction qui fait la rotation circulaire vers la gauche des elements d'une liste"
  (if (endp (cdr l))
      l
      (append (cdr l) (cons (car l) '()))))

(rotate-left *l*)

;; 4.4 
(defun rotate-right (l)
  "Une fonction qui fait la rotation circulaire vers la droite des elements d'une liste"
  (if (endp (cdr l))
      l
      (append (last l) (butlast l))))

(rotate-right *l*)

;; 4.5
(defparameter *prop* '(large red shiny cube -vs- small shiny red four-sided pyramid))

;; 4.5.1
(defun right-side (l)
  (cdr (member '-vs- l)))

;; 4.5.2
(defun left-side (l)
  (nreverse (right-side (reverse l))))

;; bai lam them
(defun print-diff (l)
  (let ((r (right-side l))
	(l (left-side l)))
    (format t "~A~&~A" (set-difference r (intersection l r)) (set-difference l (intersection l r)))))



;; 4.5.3
(defun compare (l)
  (cons (length (intersection (right-side l) (left-side l)))
	'(PROPRIETES COMMUNES)))

(compare *prop*)

;; 4.6.1
(defun my-length (l)
  (if (endp l)
      0
      (1+ (my-length (cdr l)))))

;; 4.7.1
(defun randomize-list (l n)
  (if (endp l)
      '()
      (cons (random n) (randomize-list (cdr l) n))))

(defun randomize-list-bis (l n)
  (mapcar (lambda (x)
	    (declare (ignore x))
	    (random n))
	  l))
;; 4.7.2
(defun random-list (length n)
  "Retourner une liste de longueur length composee d'entiers aleatoires pris dans [0,n["
  (randomize-list (make-list length) n))
  

(defun test-randomize-list-once (m n)
  (let ((l (random-list m n)))
    (assert (= (length l)))
    (assert (every (lambda (x) (< -1 x n) ) l))))

(defun make-my-list (l1 l2) 
  (assert (= (length l1) (length l2)))
  (sort (append (copy-list l1) (copy-list l2)) '<=))

(defun make-my-list-bis (l1 l2)
  (dotimes (x (length l2))
    (push (nth x l2) l1))
  (sort l1 #'<=))

(defun make-new-list (l l1 l2)
  (assert (= (length l1) (length l2)))
  (setf l '())
  (dotimes (x (length l1))
    (push (nth x l1) l)
    (push (nth x l2) l))
  (nreverse l))

(defun symetrie (l)
  (if (or (endp l) (= (length l) 1))
      nil
      (if (eql (car l) (car (last l)))
	  (symetrie (butlast (cdr l)))
	  nil)))
      
      

      
      
