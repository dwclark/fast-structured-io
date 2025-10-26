(in-package :fast-structured-io-properties)

(defparameter *props-loc* "/home/david/common-lisp/fast-structured-io/data/properties/one.properties")
(defparameter *props-str* (uiop:read-file-string *props-loc*))

(declaim (inline whitespace-p eol-p))

(defun whitespace-p (c)
  (declare (type character c))
  (or (char= #\Space c) (char= #\Tab c) (char= #\Formfeed c)))

(defun eol-p (c)
  (declare (type character c))
  (or (char= #\Newline c) (char= #\Return c)))

(defun remove-properties-escapes (buf start end)
  (declare (optimize (speed 0) (debug 3)))
  (let ((ret (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
	(idx start)
	(c (schar buf start)))
    (labels ((move (&optional (spaces 1))
	       (incf idx spaces)
	       (if (< idx end)
		   (setf c (schar buf idx))))
	     (skip-whitespace ()
	       (loop while (whitespace-p c)
		     do (move))))
      (loop while (< idx end)
	    do (case c
		 (#\\
		  (move)
		  (case c
		    (#\u (vector-push-extend (unicode-escape->char buf idx) ret) (move 5))
		    
		    (#\Return
		     (move)
		     (if (char= #\Newline c)
			 (move))
		     (skip-whitespace))
		    
		    (#\Newline (move) (skip-whitespace))
		    
		    (otherwise (vector-push-extend c ret) (move))))
		 
		 ((#\Return #\Newline) (error "invalid eol"))
		 
		 (otherwise (vector-push-extend c ret) (move)))))
    ret))

(defun hash-table-accum ()
  '((:on-eof (table) table)
    (:on-line (table buf key-start key-end key-escape value-start value-end value-escape)
     (let ((key (if key-escape
		    (remove-properties-escapes buf key-start key-end)
		    (subseq buf key-start key-end)))
	   (val (if value-escape
		    (remove-properties-escapes buf value-start value-end)
		    (subseq buf value-start value-end))))
       (setf (gethash key table) val)
       table))))

(defun noops ()
  '((:on-eof (table) nil)
    (:on-line (table buf key-start key-end key-escape value-start value-end value-escape) nil)))
