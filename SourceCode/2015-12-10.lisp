(defun leaves (tree)
  (if (atom tree)
      (list tree)
      (append (leaves (car tree))
	      (leaves (cdr tree)))))

(defun leaves2 (tree)
  (let ((l '()))
    (labels ((aux (tree)
	       (if (atom tree)
		   (push tree l)
		   (progn
		     (aux (cdr tree))
		     (aux (car tree))))))
      (aux tree)
      l)))
	      

(defun list-keys (ht)
  (let ((keys '()))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (push k keys))
     ht)
    keys))


(defun list-values (ht)
  (let ((values '()))
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (push v values))
     ht)
    values))


(defun print-couple (couple s)
  (format s "[~A,~A]" (first couple) (second couple)))

(defun print-couples (couples s)
  (flet ((print-couple (couple)
	   (format s "[~A,~A]~%" (first couple) (second couple))))
    (dolist (couple couples)
      (print-couple couple))))

(defun f (l)
  (dolist (e l 'fin)
    (when (null e)
      (return-from f 'echap))))

(let ((x 0))
  (defun ticket ()
    (incf x))
  (defun raz ()
    (setq x 0)))

(defun ncirc (l)
  (nconc l l))

(defun circ (l)
  (ncirc (copy-list l)))

(defun affiche-number (n *print-base*)
  (format t "~A" n))
