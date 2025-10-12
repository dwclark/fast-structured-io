(in-package :fast-structured-io)

(defun csv-standard-chars ()
  '((:separator-char #\,)
    (:quote-char #\")
    (:escape-char #\")))

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
    
    (:on-escaped-field (vec buf start end)
     (progn
       (let* ((idx (1- (fill-pointer vec)))
	      (target-vec (aref vec idx)))
	 (vector-push-extend (subseq buf start end) target-vec)
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

