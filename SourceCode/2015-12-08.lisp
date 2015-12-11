(defun rouge (n)
  (+ (* n 3) 5))

(defun composition (f g)
  (lambda (x)
    (funcall g (funcall f x))))

(defun f (n)
  (let* ((v1 (sqrt n))
	 (v2 (log v1)))
    (* (+ v1 v2) (- v1 v2))))

(defun greater-than (l x)
  "list of elements of l which are > x"
  (if (endp l)
      '()
      (let ((e (car l))
	    (ll (greater-than (cdr l) x)))
      (if (> e x)
	  (cons e ll)
	  ll))))

(defun my-length (l)
  (if (endp l)
      0
      (1+ (my-length (cdr l)))))

(defun fact-naive (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun fact-aux (n p)
  (if (zerop n)
      p
      (fact-aux (1- n) (* n p))))

(defun fact (n)
  (fact-aux n 1))

(defun my-length-aux (l s)
  (if (endp l)
      s
      (my-length-aux (cdr l) (1+ s))))

(defun my-length (l)
  (labels ((aux (l s)
	     (if (endp l)
		 s
		 (aux (cdr l) (1+ s)))))
    (aux l 0)))

(defun iota (n)
  (reverse (riota n)))

(defun iota-aux (n l)
  (if (zerop n)
      l
      (iota-aux (1- n) (cons (1- n) l))))

(defun iota (n)
  (iota-aux n '()))

(defun riota (n)
  (if (zerop n)
      '()
      (cons n (riota (1- n)))))

(defun iota (n)
  (reverse (riota n)))

(defun iota-q (n) ;;(append l (list e))
  (if (zerop n)
      '()
      (append (iota-q (1- n)) (list n))))

(defun leaves (tree)
  (if (atom tree)
      (list tree)
      (append
       (leaves (car tree))
       (leaves (cdr tree)))))

(defun leaves2 (tree)
  (let ((l '()))
    (labels ((aux (tree)
	       (if (atom tree)
		   (push tree l)
		   (progn
		     /....
		     ....
		     ))))
      (aux tree))))
