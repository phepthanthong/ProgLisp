(ql:quickload "ltk")

(defun hello-1()
  (with-ltk ()
  (let ((b (make-instance 'button
   :master nil
   :text "Press Me"
   :command (lambda ()
      (format t "Hello World!~&")))))
    (pack b))))

(defun hello-2()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
	   (b1 (make-instance 'button
			       :master f
			       :text "Button 1"
		
	       :command (lambda () (format t "Button1~&"))))
	   (b2 (make-instance 'button
			       :master f
			       :text "Button 2"
			       :command (lambda () (format t "Button2~&")))))
      (pack f)
      (pack b1 :side :left)
      (pack b2 :side :left)
      (configure f :borderwidth 3)
      (configure f :relief :sunken)
      )))

(defun canvastest()
  (with-ltk ()
    (let* ((sc (make-instance 'scrolled-canvas))
	   (c (canvas sc))
	   (line (create-line c (list 100 100 400 50 700 150)))
	   (polygon (create-polygon c (list 50 150 250 160 250
					    300 50 330 )))
	   (text (create-text c 260 250 "Canvas test")))
      (pack sc :expand 1 :fill :both)
      (scrollregion c 0 0 800 800)
      )))

(defun rect()
  (with-ltk()
    (let* ((f (make-instance 'frame))
	   (sc (make-instance 'scrolled-canvas
			      :master f))
	   (c (canvas sc))
	   (rec (create-rectangle c 60 60 200 200)))
      (pack f)
      (configure f :borderwidth 3)
      (configure f :relief :sunken)
      (pack sc)
      (scrollregion c 0 0 100 100)
      (itemconfigure c rec :fill :red))))

(defun rect1()
  (with-ltk()
    (let* ((f (make-instance 'frame))
	   (sc (make-instance 'canvas
			      :master f
			      :height 400
			      :width 300))
	   (rec (create-rectangle sc 145 380 165 400)))
      (pack f)
      (configure f :borderwidth 3)
      (configure f :relief :sunken)
      (pack sc)
      (scrollregion sc 0 0 100 100)
      (itemconfigure sc rec :fill :red))))
	  
(defun display-image (filename)
  (with-ltk ()
    (format-wish "package require Img")
    (let* ((img (make-image))
           (c (make-instance 'canvas :height 50 :width 50)) )
      ;; Pack the canvas
      (pack c)
      ;; Load the image from the file
      (image-load img filename)
      ;; Draw the image on the canvas
      (create-image c 0 0 :image img) )))

(defun k()
  (with-ltk ()
    (let* ((input (make-instance 'entry :width 20))
	   (label (make-instance 'label :text "Gol Messi"))
	   (frame (make-instance 'frame))
	   (button (make-instance 'button 
				  :master frame 
				  :text "Accept")))
      (bind label 
	    "<Enter>" 
	    (lambda (x)
	      (declare (ignore x))
	      (configure label :foreground "green")))
      (bind label
	    "<Leave>"
	    (lambda (x)
	      (declare (ignore x))
	      (configure label :foreground "red")))
      (bind button 
	    "<Button-1>" 
	    (lambda (x) 
	      (declare (ignore x)) 
	      (do-msg "I love you Maria")))
      (bind label 
	    "<Double-Button-1>" 
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "Double click the mouse")))
      (bind button 
	    "<Button-3> "
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg" Right mouse button ")))
      (bind button 
	    "<Button-2>" 
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "Middle mouse button")))
      (bind input 
	    "<Key-a>" 
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "You pressed the a key ")))
      (bind input 
	    "<Key-t>" 
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "You pressed the t key ")))
      (bind input 
	    "<Control-Key-t>" 
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "You pressed Ctrl + t")))
      (bind input 
	    "<Control-Alt-Key-m> "
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "You pressed Ctrl + Alt + m")))
      (bind input 
	    "<Escape>" 
	    (lambda (x)
	      (declare (ignore x))
	      (do-msg "You pressed the Escape key")))
      (pack label)
      (pack input)
      (pack frame)
      (pack button)
      (configure frame :borderwidth 3)
      (configure frame :relief :raised))))            

(defun scribble ()
  (with-ltk ()
    (let* ((canvas (make-instance 'canvas))
	   (down nil))
      (pack canvas)
      (bind canvas "<ButtonPress-1>"
	    (lambda (evt)
	      (setf down t)
	      (create-oval canvas
			   (- (event-x evt) 10) (- (event-y evt) 10)
			   (+ (event-x evt) 10) (+ (event-y evt) 10))))
      (bind canvas "<ButtonRelease-1>" (lambda (evt)
					 (declare (ignore evt))
					 (setf down nil)))
      (bind canvas "<Motion>"
	    (lambda (evt)
	      (when down
		(create-oval canvas
			     (- (event-x evt) 10) (- (event-y evt) 10)
			     (+ (event-x evt) 10) (+ (event-y evt) 10))))))))
