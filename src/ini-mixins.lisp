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
  (let ((cell (alists-k/v a)))
    (setf (car cell) k))
  a)

(defun alists-add-value (a v)
  (declare (type alists a))
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

(defun hashtable-init ()
  (let* ((table (make-hash-table :test #'equal)))
    (setf (gethash :current-group table) "")
    (setf (gethash :current-key table) "")
    (setf (gethash "" table) (make-hash-table :test #'equal))
    table))

(defun hashtable-accum ()
  '((:on-group (table buf start end)
     (let ((group-name (subseq buf start end)))
       (setf (gethash :current-group table) group-name)
       (setf (gethash group-name table) (make-hash-table :test #'equal))
       table))
    
    (:on-key (table buf start end escape)
     (progn
       (setf (gethash :current-key table)
	     (if escape (remove-escapes buf start end escape) (subseq buf start end)))
       table))
    
    (:on-value (table buf start end escape)
     (let* ((key (gethash :current-key table))
	    (value (if escape (remove-escapes buf start end escape) (subseq buf start end)))
	    (group-name (gethash :current-group table))
	    (group-hash (gethash group-name table)))
       (setf (gethash key group-hash) value)
       table))

    (:on-eof (table)
     (progn
       (remhash :current-group table)
       (remhash :current-key table)
       (let ((default-table (gethash "" table))
	     (count (hash-table-count table)))
	 (cond ((and (= 1 count) (< 0 (hash-table-count default-table)))
		default-table)

	       ((= 0 (hash-table-count default-table))
		(remhash "" table)
		table)
	       
	       (t
		table)))))))

   
