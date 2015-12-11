;;;;;;;;;; Exercice 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Exercice 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-first-last (l)
  (if (endp (cdr l))
      l
      (append (last l) (cdr (butlast l)) (list (first l)))))

(swap-first-last '(YOU CANT BUY LOVE))
;; (LOVE CANT BUY YOU)

;;;;;;;;;; Exercice 3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rotate-left (l)
  (if (endp (cdr l))
      l
      (append (cdr l) (list (first l)))))

;; (rotate-left '(a b c d e))
;; (B C D E A)

;;;;;;;;;; Exercice 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rotate-right (l)
  (append (last l) (butlast l)))

;; (rotate-right '(a b c d e))
;; (E A B C D)

;;;;;;;;;; Exercice 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *prop*
  '(large red shiny cube -vs- small shiny red four-sided pyramid))

(defun right-side (l)
  (cdr (member '-vs- l)))

(right-side *prop*)
;; (SMALL SHINY RED FOUR-SIDED PYRAMID)

(defun left-side (l)
  (nreverse (right-side (reverse l))))

;; (left-side *prop*)
;; (LARGE RED SHINY CUBE)

(defun compare (l)
  (cons
   (length (intersection (left-side l) (right-side l)))
   '(PROPRIETES COMMUNES)))

;; (compare *prop*)
;; (2 PROPRIETES COMMUNES)

;; (compare '(small red metal cube -vs- red plastic small cube))
;; (3 PROPRIETES COMMUNES)

;;;;;;;;;; Exercice 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-length (l)
  (if (endp l)
      0
      (1+ (my-length (cdr l)))))

;; CL-USER> (time (length (make-list 65000)))
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.001 seconds of user run time
;;   0.0 seconds of system run time
;;   0 page faults and
;;   520,192 bytes consed.
;; 65000
;; CL-USER> (time (my-length (make-list 65000)))
;; Evaluation took:
;;   0.006 seconds of real time
;;   0.005999 seconds of user run time
;;   0.0 seconds of system run time
;;   0 page faults and
;;   516,096 bytes consed.
;; 65000
;; sur ma machine (my-length (make-list 65267)) donne
;; Control stack exhausted (no more space for function call frames).  
;; This is probably due to heavily nested or infinitely recursive function 
;; calls, or a tail call that SBCL cannot or has not optimized away.
;;   [Condition of type SB-KERNEL::CONTROL-STACK-EXHAUSTED]

;;;;;;;;;; Exercice 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun randomize-list (l n)
  (mapcar (lambda (x)
	    (declare (ignore x))
	    (random n))
	  l))

(defun randomize-list (l  n)
  (if (endp l)
      '()
      (cons (random n) (randomize-list (cdr l) n))))

(defun random-list (length n)
  (randomize-list (make-list length) n))

(defun test-randomize-list-once (m n)
  (let ((l (random-list m n)))
    (assert (= (length l) m))
    (assert (every (lambda (x) (< -1 x n)) l))))


	


