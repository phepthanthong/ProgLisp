;; 5.1
(defun mapcar-fill (f l1 l2)
  (cond ((endp l1) l2)
	((endp l2) l1)
	(t (cons (funcall f (car l1) (car l2))
		 (mapcar-fill f (cdr l1) (cdr l2))))))

;; 5.2.1
(defun mappend (fun list)
  (append (funcall fun (car list))
	  (mappend fun (cdr list))))

(mappend (lambda (x) (list x (1+ x))) '(5 3 9 2 5))

;; 5.2.3
(defun mappend1 (fun list)
  (reduce #'append (mapcar fun list)))

;; 5.3
(defun next-line (l)
  (mapcar #'+ (cons 0 l) (append l (list 0))))
(defun triangle (n)
  (labels ((triangle-aux (n)
	     (if (zerop n)
		 (list (list 1))
		 (cons (next-line (car (triangle-aux (1- n))))
		       (triangle-aux (1- n))))))
    (nreverse (triangle-aux n))))

;; 5.4.1
(defun partition (pred l)
  (if (endp l)
      (list () ())
      (let ( (ll (partition pred (cdr l))) )
	(if (funcall pred (car l))
	    (list (cons (car l) (car ll)) (cadr ll))
	    (list (car ll) (cons (car l) (cadr ll)))))))

(partition (lambda (x) (>= x 3)) '(1 2 3 4 5))

;; 5.4.2
(defun quicksort (less-than l)
  (if (endp l)
      '()
      (let ((parti (partition (funcall less-than (car l)) l)))
	(append (quicksort less-than (car parti)) (list (car l)) (quicksort less-than (cadr parti))))))

(defun quicksort1 (less-than l)
  (cond ((endp l) '())
	((endp (cdr l)) l)
	(t (nconc (quicksort less-than
			     (car (partition (lambda (x) (funcall less-than x (car l)))
					     (cdr l))))
		  (cons (car l)
			(quicksort less-than (cadr (partition 
						    (lambda (x) (funcall less-than x (car l)))
						    (cdr l)))))))))
;;
;; Merge sort
;;
(defun append-partition (l1 l2) ; enter 2 sorted lists
  (if (and (not (endp l2)) (not (endp l2)))
      (if (<= (car l1) (car l2))
	  (cons (car l1) (append-partition (cdr l1) l2))
	  (cons (car l2) (append-partition l1 (cdr l2))))
      (if (endp l1)
	  l2
	  l1)))

(defun mergesort (l)
  (if (or (endp l) (endp (cdr l)))
      l
      (let ((middle (floor (/ (length l) 2))))
	(append-partition 
	 (mergesort (subseq l 0 middle))
	 (mergesort (subseq l middle))))))
	  
;;
;; Insertion sort
;;
(defun insert (phantu mang &optional (key #'<=))
  (if (endp mang)
      (list phantu)
      (if (funcall key phantu (car mang))
	  (cons phantu mang)
	  (cons (car mang) (insert phantu (cdr mang) key)))))

(defun insertion-sort (mang &optional (key #'<=))
  (if (endp mang)
      '()
      (insert (car mang) (insertion-sort (cdr mang) key) key)))

(insertion-sort '(2 4 6 1 9 5 0 6 4) #'=)
		     
;; 5.5.1


;; 5.5.6
(defun my-union (s1 s2 &key (test #'eql))
  (union s1 s2 :test test))

(defun my-intersection (s1 s2 &key (test #'eql))
  (intersection s1 s2 :test test))

(defun my-set-difference (s1 s2 &key (test  #'eql))
  (set-difference s1 s2 :test test))

(defun set-inclusion (s1 s2 &key (test  #'eql))
  (subsetp s1 s2 :test test))

(defun set-equal (s1 s2 &key (test  #'eql))
  (and (set-inclusion s1 s2 :test test)
       (set-inclusion s2 s1 :test test)))

;; 5.6.7 
(defun set-from-list-bis (l)
  (if (endp l)
      '()
      (cons (car l) (set-from-list-bis (remove (car l) (cdr l))))))

(defun set-intersection (s1 s2)
  (if (and (not (endp s1)) (not endp s2))
      (if 


;; 5.6.2
(defun uncompress (l)
  (if (endp l)
      '()
      (let ((ll (list)))
	(dotimes (x (cdar l))
	  (push (caar l) ll))
	(append ll (uncompress 
