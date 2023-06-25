;;; EQUALINE Mission 23 solver

(defstruct kifu
  (boards (list #(1 :+ 1 :+ 1 :+ 1 :+ 1))) ; 盤面のリスト (逆順)
  (routes nil)                             ; 指し手(int list)のリスト (逆順)
  )

(defun print-kifu (k)
  (loop for b in (reverse (kifu-boards k))
	for r in (cons nil (reverse (kifu-routes k)))
	for i from 0
	do (format t "#~d: ~{~a~}~%~13@{-~}~%" i r t)
	   (format t "~{~3@{|~2@a ~}|~%~}~13@{-~}~%" (coerce b 'list) t)))

(defun update-board (b r)
  (let ((nb (copy-seq b)))
    (dolist (pos r)
      (let ((e #0=(aref nb pos)))
	(if (numberp e)
	    (incf #0#)
	    (setf #0# (case e (:+ :-) (:- :*) (:* :+)
			    (t (error "unknown op: ~s" e)))))))
    nb))

(defun update-kifu (k r)
  (let ((nb (update-board (car #0=(kifu-boards k)) r)))
    (make-kifu :boards (cons nb #0#)
	       :routes (cons r (kifu-routes k)))))

(defun search-route (target board)
  (labels ((rec (pos v prev acc)
	     (let* ((c (aref board pos))
		    (v (if (numberp c)
			   (case prev
			     (:+ (+ v c))
			     (:- (- v c))
			     (:* (* v c))
			     (t (error "wrong board or route: ~s ~s"
				       board (reverse acc))))
			   v)))
	       (append
		(and (numberp c) (= v target) (list (reverse acc)))
		(loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
		      append
		      (let* ((nx (+ (mod pos 3) dx))
			     (ny (+ (truncate pos 3) dy))
			     (npos (+ nx (* ny 3))))
			(and (<= 0 nx 2) (<= 0 ny 2) (not (find npos acc))
			     (rec npos v c (cons npos acc)))))))))
    (loop for pos from 0 to 8 by 2
	  append (rec pos 0 :+ (list pos)))))

(defun solve (k stgmax)
  (labels ((rec (k stgno)
	     (let ((target (* stgno (1+ stgno) 1/2)))
	       (loop for r in (search-route target (car (kifu-boards k)))
		     for nk = (update-kifu k r)
		     append (if (< stgno stgmax)
				(rec nk (1+ stgno))
				(list nk))))))
    (rec k 3)))


(defun mhd (a b)
  (+ (abs (- (mod a 3) (mod b 3)))
     (abs (- (truncate a 3) (truncate b 3)))))

(defun count-moves (k)
  (loop for (a b) on (cons 4 (apply #'append (reverse (kifu-routes k))))
	if b sum (mhd a b)))

(defparameter *initial-states*
  (let* ((k0 (make-kifu))
	 (k1 (update-kifu (update-kifu k0 '(0)) '(0 1 4)))
	 (k2 (update-kifu (update-kifu k0 '(0)) '(0 1 2)))
	 (k3 (update-kifu (update-kifu k0 '(4)) '(4 1 0))))
    (list k1 k2 k3)))

(time
 (let* ((stgmax #+sbcl (if #0=(cadr sb-ext:*posix-argv*)
		 	   (max 3 (parse-integer #0#))
			   10)
		#-sbcl 10)
	(allans (sort (loop for k in *initial-states*
			    append (solve k stgmax))
		      #'< :key #'count-moves))
	(shortest (car allans)))
   (format t "found ~A solutions~%" (length allans))
   (format t "shortest solution: ~A moves~%" (count-moves shortest))
   (print-kifu shortest)))
