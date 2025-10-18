(in-package :fast-structured-io-csv)

(defun chars (s q e)
  (list (list :separator-char s)
	(list :quote-char q)
	(list :escape-char e)))

(defun noops ()
  '((:on-empty-field (tab) nil)
    (:on-field (tab buf start end) nil)
    (:on-escaped-field (tab buf start end escape) nil)
    (:on-line (tab) nil)
    (:on-eof (tab) nil)))

(defun remove-escapes (buf start end escape)
  (loop with ret = (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
	for idx from start below end
	do (if (char-equal (aref buf idx) escape)
	       (incf idx))
	   (vector-push-extend (aref buf idx) ret)
	finally (return ret)))

(defun matrix-accum ()
  '((:on-empty-field (vec)
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

(declaim (inline table-has-headers table-row-count table-headers table-transformers table-header-map table-rows table-current
		 table-add-field table-get-row table-get-field))

(defstruct table
  (has-headers nil :type boolean)
  (row-count 0 :type fixnum)
  (headers nil :type (vector simple-string *))
  (transformers nil :type (vector function *))
  (header-map nil :type nullable-hash-table)
  (rows nil :type (vector (vector t *) *))
  (current (make-array 0 :adjustable t :fill-pointer t) :type (vector t *)))

(defparameter *empty-array* (make-array 0))

(defun table-new-header-map (headers)
  (if (not (eq *empty-array* headers))
      (loop with new-table = (make-hash-table :test 'equal :size (length headers))
	    for header across headers
	    for idx from 0 below (length headers)
	    do (setf (gethash header new-table) idx)
	    finally (return new-table))
      nil))

(defun table-new (&key (has-headers nil) (keep-rows t) (headers *empty-array*) (transformers *empty-array*))
  (make-table :has-headers has-headers
	      :row-count (if has-headers -1 0)
	      :headers headers 
	      :transformers transformers
	      :header-map (table-new-header-map headers)
	      :rows (if keep-rows (make-array 0 :element-type '(vector simple-string *) :adjustable t :fill-pointer t) *empty-array*)))

(defun table-add-field (tab s)
  (declare (type simple-string s))
  (let ((target (table-current tab)))
    (if (= -1 (table-row-count tab))
	(vector-push-extend s target)
	(let* ((trans (table-transformers tab))
	       (func (if (not (eq *empty-array* trans))
			 (aref trans (fill-pointer target))
			 nil))
	       (val (if func (funcall func s) s)))
	       
	  (vector-push-extend val target))))
  tab)

(defun table-get-row (tab row)
  (aref (table-rows tab) row))

(defun table-get-field (tab field &optional (row -1))
  (declare (type tab table))
  (let* ((target (if (= -1 row) (table-current tab) (aref (table-rows tab) row)))
	 (column (if (typep field 'integer) field (gethash field (table-header-map tab))))
	 (ret (aref target column)))
    (aref target column)))

;; todo fix missing first row for test
(defun table-finalize-current (tab)
  (declare (type tab table))
  (let ((row (table-current tab)))
    (when (< 0 (fill-pointer row))
      (cond ((and (= -1 (table-row-count tab))
		  (table-has-headers tab)
		  (eq (table-headers tab) *empty-array*))
	     (setf (table-headers tab) (make-array (length row) :element-type 'simple-string :initial-contents row))
	     (setf (table-header-map tab) (table-new-header-map row)))
	    
	    ((array-has-fill-pointer-p (table-rows tab))
	     (vector-push-extend (make-array (length row) :initial-contents row) (table-rows tab))))
      
      (incf (table-row-count tab))
      (setf (fill-pointer row) 0)))
  tab)

(defun table-accum ()
  '((:on-empty-field (tab) (table-add-field tab ""))
    (:on-field (tab buf start end) (table-add-field tab (subseq buf start end)))
    (:on-escaped-field (tab buf start end escape) (table-add-field tab (remove-escapes buf start end escape)))
    (:on-line (tab) (table-finalize-current tab))
    (:on-eof (tab) (table-finalize-current tab))))
