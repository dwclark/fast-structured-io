(in-package :fast-structured-io)

(declaim (inline accum-list-init accum-list accum-list-extract))

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
	  

