(defun f (l)
  (dotimes (i (length l))
    (format t "i=~A : ~A~%" i (car l))
    (pop l)))

(defun g (a b &optional c (d 0))
  (list a b c d))

(defun h (a &rest l)
  (list a l))


(defun k (&key couleur image (largeur 100) (hauteur 100))
  (list couleur image largeur hauteur))

(defun fact (n)
  (assert (integerp n))
  (assert (>= n 0))
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun zeros (n)
  (make-list n :initial-element 0))

(defun test-zeros (n)
  (let ((l (zeros n)))
    (assert (listp l))
    (assert (= (length l) n))
    (assert (every #'integerp l))
    (assert (every #'zerop l))))
