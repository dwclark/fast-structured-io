(in-package :fast-structured-io-ini)

;(declaim (optimize (speed 0) (debug 3)))
(declaim (inline alists-group alists-group-name alists-k/v alists-all alists-add-key
		 alists-add-value alists-add-group-name alists-finish-group alists-finish))

(defstruct alists
  (group (accum-list-init) :type cons)
  (group-name "" :type simple-string)
  (k/v (cons "" "") :type cons)
  (all (accum-list-init) :type cons))

(defun alists-add-key (a k)
  (declare (type alists a))
  (format t "in alists-add-key ~A~%" k)
  (let ((cell (alists-k/v a)))
    (setf (car cell) k))
  a)

(defun alists-add-value (a v)
  (declare (type alists a))
  (format t "in alists-add-value ~A~%" v)
  (let ((cell (alists-k/v a)))
    (setf (cdr cell) v)
    (setf (alists-group a) (accum-list (alists-group a) cell))
    (setf (alists-k/v a) (cons "" "")))
  a)

(defun alists-finish-group (a)
  (declare (type alists a))
  (let ((finished (accum-list-extract (alists-group a))))
    (when finished
      (let ((new-entry (cons (alists-group-name a) finished)))
	(setf (alists-all a) (accum-list (alists-all a) new-entry)))))
  a)

(defun alists-add-group-name (a n)
  (declare (type alists a))
  (alists-finish-group a)
  (setf (alists-group-name a) n)
  (setf (alists-group a) (accum-list-init))
  a)

(defun alists-extract-global-only (alist)
  (if (and (not (rest alist))
	   (assoc "" alist :test #'string=))
      (rest (car alist))
      alist))
  
(defun alists-finish (a)
  (alists-finish-group a)
  (alists-extract-global-only (accum-list-extract (alists-all a))))

(defun alists-accum ()
  '((:on-group (al buf start end) (alists-add-group-name al (subseq buf start end)))
    (:on-key (al buf start end escape) (alists-add-key al
					(if escape
					    (remove-escapes buf start end escape)
					    (subseq buf start end))))
    (:on-value (al buf start end escape) (alists-add-value al
					  (if escape
					      (remove-escapes buf start end escape)
					      (subseq buf start end))))
    (:on-eof (al) (alists-finish al))))

(defun hashtables-accum ())

   
