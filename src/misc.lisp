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
	  

