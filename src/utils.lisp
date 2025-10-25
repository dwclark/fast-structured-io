(in-package :fast-structured-io-utils)

;; types
(deftype nullable-hash-table () '(or hash-table null))

;; list helpers
(declaim (inline accum-list-init accum-list accum-list-extract unicode-escape->char))

(defun accum-list-init ()
  (cons nil nil))

(defun accum-list (cell contents)
  (let ((next-cell (cons contents nil)))
    (cond
      ((cdr cell)
       (setf (cdr (cdr cell)) next-cell)
       (setf (cdr cell) next-cell))
      (t
       (setf (car cell) next-cell)
       (setf (cdr cell) next-cell)))
    cell))

(defun accum-list-extract (cell)
  (car cell))
	  
;; mixin helpers
(defun replace-symbol (lst src target)
  (loop for sub on lst
	do (let ((item (car sub)))
	     (cond ((atom item)
		    (if (eq item src)
			(rplaca sub target)))
		   ((consp item)
		    (rplaca sub (replace-symbol item src target)))))
	finally (return lst)))
    
(defun replace-symbols (lst srcs targets)
  (loop with copy = (copy-tree lst)
	for src in srcs
	for target in targets
	do (setf copy (replace-symbol copy src target))
	finally (return copy)))

(defun mixin-or-default (spec name default)
  (let ((found (rest (assoc name spec))))
    (if found (first found) default)))

(defun mixin-type (spec name)
  (mixin-or-default spec name 't))

(defun mixin-call (spec name &rest syms)
  (let* ((found (rest (assoc name spec)))
	 (arg-list (first found))
	 (body (first (rest found))))
    (cond ((null arg-list)
	   body)

	  ((atom body)
	   (if (eq body (first arg-list))
	       (first syms)
	       body))
	  
	  (t
	   (replace-symbols body arg-list syms)))))

(defun remove-escapes (buf start end escape)
  (loop with ret = (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
	for idx from start below end
	do (if (char-equal (aref buf idx) escape)
	       (incf idx))
	   (vector-push-extend (aref buf idx) ret)
	finally (return ret)))

(defun unicode-escape->char (buf start)
  (code-char (parse-integer buf :start (+ 1 start) :end (+ 5 start) :radix 16)))
