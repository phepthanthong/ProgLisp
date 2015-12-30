;;
;; Bai Bonus
;;

;; 1
(defun search-atom-recurs (a l)
  "Search an atom in a list"
  (if (endp l)
      nil
      (if (equal a (car l))
	  t
	  (search-atom a (cdr l)))))

(search-atom-recurs 'x '(x2 x 1))
(search-atom-recurs 'x3 '(x2 x 1))

(defun search-atom-ite (a l)
  (let ((w nil))
    (dotimes (x (length l))
      (if (equal a (nth x l))
	  (setq w t)))
    w))

(search-atom-recurs 'x '(x2 x 1))
(search-atom-recurs 'x3 '(x2 x 1))

;; 2
(defun compare (e l)
  (if (endp l)
      
      nil))

(defun duplicate-member (l)
  "return True if there is an duplicated member, False for otherwise"
  (if (compare (car l) (cadr l))
      t
      (duplicate-member (cdr l))))

;; 3
(defun random-list-recurs (n l)
  (if (zerop n)
      '()
      (cons (nth (random (length l)) l) (random-list-recurs (1- n) l))))

(random-list-recurs 3 '(a b c d e f g h))

(defun random-list-ite (n list)
  (let ((size (length list))
	(l '()))
    (dotimes (i n)
      (push (nth (random size) list) l))
    l))

(random-list-ite 3 '(a b c d e f g h))

;; 4
(defun make-noun-phrase (ht)
  (let ((l '())
	(np1 (gethash 'noun-phrase1 ht))
	(np2 (gethash 'noun-phrase2 ht)))
    (push (append (gethash 'article2 np2)
		  (gethash 'noun2 

(defun make-noun (ht)
  (let ((l '())
	(np1 (gethash 'noun-phrase1 ht))
	(np2 (gethash 'noun-phrase2 ht)))
    (push (gethash 

(defun make-verb (ht)
  (list (car (gethash 'verb ht))))

(defun make-sentence (ht &optional (key 'sentence))
  (let ((l-noun-phrase (make-noun-phrase ht))
	(l-verb (make-verb ht))
	(l-article (make-article ht))
	(l-noun (make-noun ht)))
    (cond 
      ((equal key 'noun-phrase) l-noun-phrase)
      ((equal key 'verb) l-verb)
      ((equal key 'noun) l-noun)
      ((equal key 'article) l-article)
      ((equal key 'sentence)
       (append (car l-noun-phrase)
	       l-verb
	       (cadr l-noun-phrase))))))
