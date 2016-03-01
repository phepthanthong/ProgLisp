(defun list-key (ht)
  (let ((keys '()))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (push k keys))
     ht)
    keys))

(defun list-value (ht)
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
	   (format s "[~A,~A]" (first couple) (second couple))))
    (dolist (couple couples)
      (print-couple couple))))

;; loop est une macro qui sert a faire les boucles (langage specialise pour boucles: embedded languages/langage enchasse)

(defun f (l)
  (dolist (e l 'fin)
    (when (null e)
      (return-from f 'echap))))

;; Declarer une variable statique
(let ((x 0))
  (defun ticket()
    (incf x))
  (defun raz ()
    (setq x 0)))

(defun ncirc (l)
  (ncons (l l)))

(defun circ (l)
  (ncirc (copy-list l)))

(defun affiche-number (n *print-base*)
  (format t "~A" n))

(adjust-array *t* '(3 3))
(fill-pointer t)
