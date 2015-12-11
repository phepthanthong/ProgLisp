;;; Exercice 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar-fill (fun l1 l2)
  (cond ((endp l1) l2)
	((endp l2) l1)
	(t (cons (funcall fun (car l1) (car l2))
		 (mapcar-fill fun (cdr l1) (cdr l2))))))

(defun test-mapcar-fill-against-mapcar (fun l1 l2)
  (assert (= (length l1) (length l2)))
  (assert (equal (mapcar-fill fun l1 l2)
		 (mapcar fun l1 l2))))

;;; mapcar général s'appliquant à un nombre quelconque de listes
(defun map-aux (g l)
  (reduce (lambda (li lli)
	    (cons (funcall g li) lli))
	  l
	  :initial-value '()
	  :from-end t))

(defun my-mapcar (f &rest l)
  (if (some #'endp l)
      '()
      (cons
       (apply f (map-aux #'car l))
       (apply #'my-mapcar f
	      (map-aux #'cdr l)))))

;;; Exercice 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1-
(defun mappend1 (fun l)
  (if (endp l)
      '()
      (append (funcall fun (car l))
	      (mappend fun (cdr l)))))

;; 2- débordement de pile
;; 3- 
(defun mappend2 (fun l)
  (apply #'append (mapcar fun l)))
;; la fonction "apply" sera appelée avec un nombre d'arguments 
;; égal à la longueur de la liste, ce qui est portable uniquement 
;; si la longueur de la liste est inférieure à 50. 

;; 4- 
(defun mappend3 (fun list)
  (reduce #'append (mapcar fun list)))

;; 5- Ça prend plusieurs secondes. 

;; 6- Maintenant ça prend quelques millisecondes.
(defun mappend4 (fun list)
  (reduce #'append (mapcar fun list) :from-end t))

;; La  version sans `:from-end t' est quadratique, car "append" sera
;; obligé de copier la même liste plusieurs fois. 

;;;;;;;;;; Exercice 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next-line (l)
  (mapcar #'+ (cons 0 l) (append l (list 0))))

(defun triangle-aux (n)
  (if (zerop n)
      '((1))
      (let ((precedent (triangle-aux (1- n))))
	(cons 
	 (next-line (car precedent))
	 precedent))))

(defun triangle (n)
  (reverse (triangle-aux n)))

;;;; Remplacer append par nconc dans next-line donne un résultat
;;;; incorrect : expliquer pourquoi
(defun triangle (n)
  (nreverse (triangle-aux n)))

;;;;;;;;;; Exercice 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  partition (pred l)
;;;   renvoie deux valeurs : la liste des elements qui vérifient pred
;;;                          la liste des elements qui ne vérifient pas pred
;;; l'ordre des éléménts est respecté

(defun partition-aux (pred l l-ok l-notok)
  (cond ((endp l) (values (nreverse l-ok) (nreverse l-notok)))
	((funcall pred (car l))
	 (partition-aux pred (cdr l) (cons (car l) l-ok) l-notok))
	(t (partition-aux pred (cdr l) l-ok (cons (car l) l-notok)))))

(defun partition (pred l)
  (partition-aux pred l '() '()))

;;;; application de partition : 

(defun quicksort (lt l)
  (cond ((endp l) '())
	((endp (cdr l)) l)
	(t (multiple-value-bind (l-inf l-sup)
	       (partition (lambda (x) (funcall lt x (car l)))
			  (cdr l))
	     (nconc (quicksort lt l-inf)
		    (cons (car l) (quicksort lt l-sup)))))))

;;; Exercice 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1
(defconstant +empty-set+ '())

;;; 2
(defun set-emptyp (s)
  (endp s))

;;; 3
(defun set-member (e s &key (test #'eql) )
  (member e s :test test))

;;; 4
(defun set-adjoin (e s &key (test #'eql))
  (adjoin e s :test test))

;;; 5
(defun set-from-list (l &key (test #'eql))
  (remove-duplicates l :test test))

;;; 6
(defun set-union (s1 s2 &key (test  #'eql))
  (union s1 s2 :test test))

(defun set-intersection (s1 s2 &key (test  #'eql))
  (intersection s1 s2 :test test))

(defun my-set-difference (s1 s2 &key (test  #'eql))
  (set-difference s1 s2 :test test))

(defun set-inclusion (s1 s2 &key (test  #'eql))
  (subsetp s1 s2 :test test))

(defun set-equal (s1 s2 &key (test  #'eql))
  (and (set-inclusion s1 s2 :test test)
       (set-inclusion s2 s1 :test test)))

;;;; 7
(defun same-edge (e1 e2)
  (or (equal e1 e2)
      (and (eql (car e1) (cadr e2))
	   (eql (cadr e1) (car e2)))))

(set-union (set-from-list '((1 2) (3 0) (2 1) (2 5)) :test #'same-edge)
	   (set-from-list '((1 2) (0 3) (22 1) (5 2)) :test #'same-edge)
	   :test #'same-edge)


;;; 8
;;; ----------------------------------------------------------
;;; a) Cette représentation ne traite que les ensembles finis.
;;;    On pourrait représenter les ensembles par des prédicats, mais
;;;    les tests d'inclusions et d'égalité deviendraient irréalisables
;;;
;;; b) Cette représentation n'interdit pas d'ajouter des éléments
;;;    à l'aide de fonctions d'égalité différentes :
;;;    (defvar *s1* (set-from-list (list '(a b) '(b) (cdr '(c a b)))
;;;                                :test #'equal))
;;;    (set-adjoin '(a b) *s1* #'eq)
;;; Solution possible: encapsuler la fonction d'égalité dans un objet 
;;; ------------------------------------------------------------
(defclass finite-set ()
   ((elements :initarg :elements :reader elements)
    (equality :initform #'equal :initarg :equality :reader equality)))

(defun make-finite-set (list &key (equality #'equal))
  (make-instance 'finite-set :elements (set-from-list list :test equality)
		 :equality equality))

(defun finite-set-combine (f1 f2  fun)
  (assert (eq (equality f1) (equality f2)))
  (make-finite-set
   (funcall fun (elements f1) (elements f2) :test (equality f1) )
   :equality (equality f1)))

(defun finite-set-inter (f1 f2)
  (finite-set-combine f1 f2 #'set-intersection))

(defun finite-set-union (f1 f2)
  (finite-set-combine f1 f2 #'set-union))

;;; Exercice 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compression d'une liste sous forme de couples 
;;; (element . facteur-de-repetition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Première solution en construisant la liste des paires à l'envers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compress-aux (l pairs test)
  (cond
    ((endp l)
     pairs)
    ((or (endp pairs) (not (funcall test (car l) (caar pairs))))
     (compress-aux
      (cdr l)
      (cons (cons (car l) 1) pairs)
      test))
    (t
     (compress-aux
      (cdr l)
      (cons (cons (car l) (1+ (cdar pairs)))
	    (cdr pairs))
      test))))

(defun compress (l &key (test #'equal))
  (nreverse (compress-aux l '() test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deuxième solution en construisant la liste des paires à l'endroit
;;; et utilisant le mécanisme de valeurs multiples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compress-aux (x n l test)
  (cond ((endp l) (values (cons x n) '()))
	((funcall test x (car l)) (compress-aux x (1+ n) (cdr l) test))
	(t (values (cons x n) l))))

(defun compress (l &key (test #'equal))
  (if (endp l)
      l
      (multiple-value-bind (frame rest)
	  (compress-aux (car l) 1 (cdr l) test)
	(cons frame (compress rest :test test)))))

;;;; decompression
(defun uncompress (l)
  (mapcan (lambda (frame) (make-list (cdr frame) :initial-element (car frame)))
	  l))
;;; test 
(defun test-compress (l)
  (equal l (uncompress (compress l))))
