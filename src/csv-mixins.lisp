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

(defstruct csv-table
  (has-headers nil :type boolean)
  (row-count 0 :type fixnum)
  (headers nil :type (vector simple-string *))
  (transformers nil :type (vector function *))
  (header-map nil :type nullable-hash-table)
  (rows nil :type (vector (vector t *) *))
  (current (make-array 0 :adjustable t :fill-pointer t) :type (vector t *)))

(defparameter *empty-array* (make-array 0))

(defun csv-table-new-header-map (headers)
  (if (not (eq *empty-array* headers))
      (loop with new-table = (make-hash-table :test 'equal :size (length headers))
	    for header across headers
	    for idx from 0 below (length headers)
	    do (setf (gethash header new-table) idx)
	    finally (return new-table))
      nil))

(defun csv-table-new (&key (has-headers nil) (keep-rows t) (headers *empty-array*) (transformers *empty-array*))
  (make-csv-table :has-headers has-headers
		  :headers headers 
		  :transformers transformers
		  :header-map (csv-table-new-header-map headers)
		  :rows (if keep-rows (make-array 0 :element-type '(vector simple-string *) :adjustable t :fill-pointer t) *empty-array*)))

(defun csv-table-add-field (table s)
  (declare (type simple-string s))
  (let ((target (csv-table-current table)))
    (if (and (zerop (csv-table-row-count table))
	     (csv-table-has-headers table))
	(vector-push-extend s target)
	(let* ((trans (csv-table-transformers table))
	       (func (if (not (eq *empty-array* trans))
			 (aref trans (fill-pointer row))
			 nil)))
	  (vector-push-extend (if func (funcall func s) s) row))))
  table)

(defun csv-table-get-field (table field &optional (row -1))
  (declare (type table csv-table))
  (let* ((target (if (= -1 row) (csv-table-current table) (aref (csv-table-rows table) row)))
	 (column (if (typep field 'integer) field (gethash field (csv-table-headers table)))))
    (aref target column)))

(defun csv-table-finalize-current (table)
  (declare (type csv-table table))
  (let ((row (csv-table-current table)))
    (when (< 0 (fill-pointer row))
      (cond ((and (= 0 (csv-table-row-count table))
		  (csv-table-has-headers table)
		  (eq (csv-table-headers table) *empty-array*))
	     (setf (csv-table-headers table) (make-array (length row) :element-type 'simple-string :initial-contents row))
	     (setf (csv-table-headers-table table) (csv-table-new-header-map row)))
	    
	    ((array-has-fill-pointer-p (csv-table-rows table))
	     (vector-push-extend (make-array (length row) :initial-contents row) (csv-table-rows table))))
      
      (setf (fill-pointer row) 0)
      (incf (csv-table-row-count table))))
  table)

(defun csv-table-accum-functions (&key (has-headers nil) (headers *empty-array*) (transformers *empty-array*))
  '((:construct-context () (csv-table-new :has-headers has-headers :keep-rows t :headers headers :transformers transformers))
    (:on-empty-field (table) (csv-table-add-field table ""))
    (:on-field (table buf start end) (csv-table-add-field table (subseq buf start end)))
    (:on-escaped-field (table buf start end escape) (csv-table-add-field table (remove-escapes buf start end escape)))
    (:on-line (table) (csv-table-finalize-current table))
    (:on-eof (table) (csv-table-finalize-current table))))
