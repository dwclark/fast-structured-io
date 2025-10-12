(in-package :fast-structured-io)

(defun csv-chars (s q e)
  (list (list :separator-char s)
	(list :quote-char q)
	(list :escape-char e)))

(defun remove-escapes (buf start end escape)
  (loop with ret = (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
	for idx from start below end
	do (if (char-equal (aref buf idx) escape)
	       (incf idx))
	   (vector-push-extend (aref buf idx) ret)
	finally (return ret)))

(defun vec-accum-functions ()
  '((:construct-context () (make-array 1 :adjustable t :fill-pointer t
					 :initial-contents (list (make-array 0 :adjustable t :fill-pointer t))))
    (:on-empty-field (vec)
     (let* ((idx (1- (fill-pointer vec)))
	    (target-vec (aref vec idx)))
       (vector-push-extend "" target-vec)
       vec))
    
    (:on-field (vec buf start end)
     (progn
       (let* ((idx (1- (fill-pointer vec)))
	      (target-vec (aref vec idx)))
	 (vector-push-extend (subseq buf start end) target-vec)
	 vec)))
    
    (:on-escaped-field (vec buf start end escape)
     (progn
       (let* ((idx (1- (fill-pointer vec)))
	      (target-vec (aref vec idx)))
	 (vector-push-extend (remove-escapes buf start end escape) target-vec)
	 vec)))
    
    (:on-line (vec)
     (progn
       (vector-push-extend (make-array 0 :adjustable t :fill-pointer t) vec)
       vec))
    
    (:on-eof (vec)
     (let* ((idx (1- (fill-pointer vec)))
	    (target-vec (aref vec idx)))
       (if (= 0 (fill-pointer target-vec))
	   (vector-pop vec))
       vec))))

