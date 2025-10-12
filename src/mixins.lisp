(in-package :fast-structured-io)

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
