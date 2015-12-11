;;;;;;;;;; Exercice 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; derivation formelle extra-light, sans data-driven, + et * binaires, sans simplifications
(defun derivee-simple (var expr)
  (cond ((eq var expr) 1)
	((atom expr) 0)
        ((eq (first expr) '+) 
         (list '+
	       (derivee-simple var (second expr)) 
	       (derivee-simple var (third expr))))
	((eq (first expr) '*)
	 (list '+
	       (list '* (second expr) (derivee-simple var (third expr)))
	       (list '* (third expr) (derivee-simple var (second expr)))))))

(derivee-simple 'x '(+ x 3))
;(+ 1 0)
(derivee-simple 'x '(+ x (+ x 3)))
;(+ 1 (+ 1 0))
(derivee-simple 'x '(+ (* 3 x) 56))
;(+ (+ (* 3 1) (* X 0)) 0)

;;;;;;;;;; Exercice 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; derivation formelle light: + et * binaires, sans simplifications
(defvar *derivateurs* (make-hash-table :test #'eq)
  "fonctions de derivation formelle")

(defun enregistrer-fonction (op deriv-fun)
  (setf (gethash op *derivateurs* ) deriv-fun)
  op)

(defun derivee-simple (var expr)
  (cond ((eq var expr) 1)
	((atom expr) 0)
	(t (funcall (gethash (first expr) *derivateurs*) var expr))))

(enregistrer-fonction
 '+
 (lambda (var expr)
   (list (car expr)
	 (derivee-simple var (second expr)) 
	 (derivee-simple var (third expr)))))

(enregistrer-fonction
 '*
 (lambda (var expr)
   (list '+
	 (list (car expr) (second expr) (derivee-simple var (third expr)))
	 (list (car expr) (third expr) (derivee-simple var (second expr))))))

;;; Avec backquote (non testé) :
;;; (enregistrer-fonction
;;;  '*
;;;  (lambda (var expr)
;;;    `(+ (* ,(second expr) ,(derivee-simple var (third expr)))
;;;        (* ,(third expr) ,(derivee-simple var (second expr)))))

(derivee-simple 'x '(+ x 3))  
;(+ 1 0)
(derivee-simple 'x '(+ x (+ x 3)))
;(+ 1 (+ 1 0))
(derivee-simple 'x '(+ (* 3 x) 56))
;(+ (+ (* 3 1) (* X 0)) 0)

;;; améliorations vers le light plus
;;; (le + n-aire, la derivée n-ième)
(enregistrer-fonction 
 'exp (lambda (var expr)
	(list '* (derivee-simple var (second expr)) expr)))

(derivee-simple 'x '(exp (+ (* 3 x) 56)))
; (* (+ (+ (* 3 1) (* x 0)) 0) (exp (+ (* 3 x) 56)))

(enregistrer-fonction 
 '+ (lambda (var expr)
      (cons (car expr) (mapcar (lambda (e) (derivee-simple var e)) (cdr expr)))))

(derivee-simple 'x '(+ x x 3 x 56))  
;(+ 1 1 0 1 0)

(defun derivee-n (var expr &key (ordre 1))
  (if (zerop ordre)
      expr
      (derivee-n var (derivee-simple var expr) :ordre (1- ordre))))

(derivee-n 'x '(+ x x 3 x 56) :ordre 2)  
;(+ 0 0 0 0 0)

;;;;;;;;;; VERSIONS SUPPLEMENTAIRES ;;;;;;;;;;;;;;;;;;;;
;;; 1 - dérivation formelle moins light (avec simplifications)
(defvar *derivateurs-1* (make-hash-table :test #'eq)
  "fonctions de derivation formelle")

(defvar *simplificateurs-1* (make-hash-table :test #'eq)
  "fonctions de simplification")

(defun simplifier-application (expr)
  (let ((simplificateur (gethash (car expr) *simplificateurs-1*)))
    (if simplificateur
	(funcall simplificateur expr)
	expr)))

;;; "apply" d'une opération à une liste d'expressions symboliques
(defun appliquer (op arg)
  (let* ((evaluable (every #'numberp arg))
	 (fun (and evaluable (fboundp op) (symbol-function op))))
    (if fun
	(apply fun arg)
	(simplifier-application (cons op arg)))))

;;; "funcall" correspondant
(defun appeler (op &rest arg)
  (appliquer op arg))

(defun simplifier (expression)
  (if (atom expression)
      expression
      (appliquer (car expression)
		 (mapcar #'simplifier (cdr expression)))))

(defun derivee-1 (var expr)
  (cond ((eq var expr) 1)
	((atom expr) 0)
	(t
	 (simplifier (funcall (gethash (car expr) *derivateurs-1*) var expr)))))

(defun derivee-n-1 (var  expr &key (ordre 1))
  (if (zerop ordre)
      expr
      (derivee-n-1 var (derivee-1 var expr) :ordre (1- ordre))))

(defun enregistrer-fun-1 (op deriv-fun &key simp-fun)
  (setf (gethash op *derivateurs-1* ) deriv-fun)
  (setf (gethash op *simplificateurs-1*) simp-fun)
  op)

(enregistrer-fun-1
 '+ (lambda (var expr)
      (appliquer '+ (mapcar (lambda (e) (derivee-1 var e)) (cdr expr))))
 :simp-fun (lambda (expr)
	     (let ((arg (remove 0 (cdr expr))))
	       (cond ((null arg) 0)
		     ((null (cdr arg)) (car arg))
		     (t (cons (car expr) arg))))))

(defun simplifier-multiplication (expr)
  (let ((arg (remove-if (lambda (x) (eql x 1)) (cdr expr))))
    (cond
      ((null arg) 1)
      ((null (cdr arg)) (car arg))
      ((find-if (lambda (x) (eql x 0)) arg) 0)
      (t (cons (car expr) arg)))))
    
(enregistrer-fun-1 
 '* (lambda (var expr)
      (appeler '+
	       (appeler '* (second expr) (derivee-1 var (third expr)))
	       (appeler '* (third expr) (derivee-1 var (second expr)))))
 :simp-fun #'simplifier-multiplication)

(derivee-1 'x '(+ x 3))  
;1
(derivee-1 'x '(+ x (+ u x) x))
;3
(derivee-1 'x '(+ (* 3 x) 56))
;3

;;; 2 dérivation formelle pas très light
;;; avec simplifications, fonctions n-aires et encore plus d'opérateurs
(defvar *derivateurs-2* (make-hash-table :test #'eq)
  "fonctions de derivation formelle")

(defvar *simplificateurs-2* (make-hash-table :test #'eq)
  "fonctions de simplification")

(defvar *standard-fun-2* (make-hash-table :test #'eq)
  "fonction lisp pour traiter les associativités")

;;; rend certaines opérations binaires (notamment *, / et - )
(defun standardiser (expr)
  (if (atom expr)
      expr
      (let* ((op (first expr))
	     (standard-fun (gethash op *standard-fun-2*))
	     (arg (mapcar #'standardiser (rest expr)))
	     (nexpr (cons op arg)))
	(if standard-fun
	    (funcall standard-fun nexpr)
	    nexpr))))

(defun derivee-standard (var expr)
  (cond ((eq var expr) 1)
	((atom expr) 0)
	(t
	 (simplifier (funcall (gethash (car expr) *derivateurs-2*) var expr)))))

(defun derivee (var expr)
  (derivee-standard var (standardiser expr)))

(defun derivee-standard-n (var expr &key (ordre 1))
  (if (zerop ordre)
      expr
      (derivee-standard-n var (derivee-standard var expr) :ordre (1- ordre))))

(defun derivee-n (var expr &key (ordre 1))
  (derivee-standard-n var (standardiser expr) :ordre ordre))

(defun enregistrer-fun-2 (op deriv-fun &key simp-fun standard-fun)
  (setf (gethash op *derivateurs-2* ) deriv-fun)
  (setf (gethash op *simplificateurs-2*) simp-fun)
  (setf (gethash op *standard-fun-2*) standard-fun)
  op)

;;; exprime opération comme opérateur associatif à gauche
(defun associativite-gauche (op arg)
  (do ((reste (cddr arg) (cdr reste))
       (result (list op (first arg) (second arg))
	       (list op result (car reste))))
      ((endp reste) result)))

;;; exprime opération comme opérateur associatif à droite
(defun associativite-droite (op arg)
  (if (endp (cddr arg))
      (cons op arg)
      (list op
	    (first arg)
	    (associativite-droite op (cdr arg)))))
	   
(enregistrer-fun-2
 '+ (lambda (var expr)
      (appliquer (car expr) (mapcar (lambda (u) (derivee-standard var u))
					(cdr expr))))
 :simp-fun (lambda (expr)
	     (let ((arg (remove 0 (cdr expr))))
	       (cond ((null arg) 0)
		     ((null (cdr arg)) (car arg))
		     (t (cons (car expr) arg))))))
(enregistrer-fun-2
 '- (lambda (var expr)
      (appeler (first expr) (derivee-standard var (second expr)) (derivee-standard var (third expr))))
 :simp-fun (lambda (expr)
	     (if (eql (third expr) 0)
		 (second expr)
		 expr))
 :standard-fun (lambda (expr)
		 (let ((arg (cdr expr)))
		   (cond ((null arg) 0)
			 ((null (cdr arg)) (list (car expr) 0 (car arg)))
			 (t (associativite-droite (car expr) arg))))))

(enregistrer-fun-2
 '* (lambda (var expr)
      (appeler '+
	       (appeler '* (derivee-standard var (second expr)) (third expr))
	       (appeler '* (second expr) (derivee-standard var (third expr)))))
 :simp-fun (lambda (expr)
	     (cond ((or (eql (second expr) 0) (eql (third expr) 0)) 0)
		   ((eql (second expr) 1) (third expr))
		   ((eql (third expr) 1) (second expr))
		   (t expr)))
 :standard-fun (lambda (expr)
		 (let ((arg (cdr expr)))
		   (cond ((null arg) 1)
			 ((null (cdr arg)) (car arg))
			 (t (associativite-gauche '* arg))))))

(enregistrer-fun-2
 '/ (lambda (var expr)
      (appeler
       '/
       (appeler '-
		(appeler '* (derivee-standard var (second expr)) (third expr))
		(appeler '* (second expr) (derivee-standard var (third expr))))
       (appeler '* (third expr) (third expr))))
 :simp-fun (lambda (expr)
	     (if (eql (second expr) 0) 0 expr))
 :standard-fun (lambda (expr)
		 (let ((arg (cdr expr)))
		   (cond ((null arg) 1)
			 ((null (cdr arg)) (list '/ 1 (car arg)))
			 ((null (cddr arg))
			  (associativite-droite '/ arg))))))

(enregistrer-fun-2
 'exp (lambda (var expr)
	(appeler '* (derivee-standard var (second expr)) (appeler 'exp (second expr)))))

(enregistrer-fun-2
 'log (lambda (var expr)
	(appeler '/ (derivee-standard var (second expr)) expr)))

(derivee 'x '(exp (+ (* 3 x) 56)))
(derivee-n 'x '(exp (+ (* 3 x) 56)) :ordre 2)
(derivee 'x '(log (+ (* 3 x) y)))
(derivee-n 'x '(log (+ (* 3 x) y)) :ordre 2)
(derivee 'x '(log (/ 1 x)))
(derivee-n 'x '(log (/ 1 x)) :ordre 2)
(derivee 'x '(log (* x x x)))
(derivee-n 'x '(log (* x x x)) :ordre 3)
(derivee-n 'x '(* x x x x) :ordre 3)
(derivee-n 'x '(* x x x x) :ordre 4)
(derivee-n 'x '(* x x x x x) :ordre 5)
