;;; Exercice 1
;; 1. ((1 . 2) 3 4)
;; 2. (cons (cons 1 2) (cons 3 (cons 4 nil)))
;; 3. (list (cons 1 2) 3 4)

;;; Exercice 2
;; 4.
(defun couples-elements (couples)
  (remove-duplicates
   (reduce #'append couples)
   :test #'=))

;;; Exercice 3
;; 5.
(defun rel-adds (couples relation)
  (if (endp couples)
      relation
      (rel-adds (cdr couples) (rel-add (car couples) relation))))
;; 6.
(defun add-reflexive-couples (elements relation)
  (rel-adds
   (mapcar (lambda (e) 
	     (make-list 2 :initial-element e))
	   elements)
   relation))

;; 7.
(defun rel-make-reflexive (relation)
  (add-reflexive-couples (rel-elements relation) relation))

;; 8
(defun rel-add (couple relation)
  (adjoin couple relation :test #'equal))

;;; Exercice 4
;; 9. 
(defun rel-member (couple relation)
  (funcall relation couple))

;; 10.
(defun rel-add (couple relation)
  (if (rel-member couple relation) ;; pas indispensable; pour plus d'efficacite
      relation
      (lambda (c)
	(if (null c)
	    (union 
	     (remove-duplicates couple)
	     (rel-elements relation))
	    (or (equal couple c)
		(rel-member c relation))))))

;; 11.
(defun rel-make-symmetric (relation)
  (lambda (c)
    (if (null c)
	(rel-elements relation)
	(or (rel-member c relation)
	    (rel-member (reverse c) relation)))))
