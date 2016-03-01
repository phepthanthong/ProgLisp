;; La fonction partition
(defun partition (l predicat)
  (if (endp l)
      (list '() '())
      (let ((p (partition (cdr l) predicat))
	    (e (car l))) ;; ((..) (..))
	(if (funcall predicat e)
	    (list (cons e (first p)) (second p))
	    (list (first p) (cons e (second p)))))))

(defun partition-aux (l pred ok nok)
  (if (endp l)
      (list (nreverse ok) (nreverse nok))
      (let ((e (car l)))
	(if (funcall pred e)
	    (partition-aux (cdr l) pred (cons e ok) nok)
	    (partition-aux ok (cons e nok))))))

(defun partition-term (l pred)
  (partition-aux l pred ('() '())))

(defun partition2 (l predicat)
  (if (endp l)
      (values '() '())
      (let ((e (car l)))
	(multiple-value-bind (ok nok) (partition2 (cdr l) predicat) ;; ((..) (..))
	  (if (funcall predicat e)
	      (values (cons e ok) nok)
	      (values ok (cons e nok)))))))
      

;; La fonction trier Quicksort
(defun quicksort (l)
  (if (endp (cdr l))
      l
      (let* ((p (car l))
	     (part (partition (cdr l) (lambda (x) (<= x p)))))
	(append 
	 (quicksort (first part))
	 (list p)
	 (quicksort (second part))))))
	
;; =====================================
;; LES MACROS
(defmacro pi! (var)
  (list 'setq var 'pi))

(defmacro pi! (var)
  `(setq ,var pi))

(defmacro set-x (exp)
  (list 'setq '*x* exp))

(defmacro set-x (exp)
  `(setq *x* ,exp))
  
(defmacro while (c &rest body)
  `(do ()
       ((not ,c))
     ,@body))

(defmacro for (init test update &body body)
  `(progn
     ,init
     (while ,test
       ,@body
       ,update)))

(defmacro ntimes (n &body body)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,y ,n))
       (do ((,x 0 (1+ ,x)))
	   ((>= ,x ,y))
	 ,@body)))))

(defmacro defpower (nom n)
  `(defun ,nom (x)
     (* ,@(make-list (eval n) :initial-element 'x))))
