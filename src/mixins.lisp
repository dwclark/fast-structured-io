(in-package :fast-structured-io)

(defun mixin-type (spec name)
  (let ((found (rest (assoc name spec))))
    (if found (first found) 't)))

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
