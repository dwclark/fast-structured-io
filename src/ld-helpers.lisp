(in-package :fast-structured-io)

(defun construct-parser-call (spec arg-sym)
  (destructuring-bind ((arg) &body body) (rest (assoc :construct-parser spec))
    (replace-symbols body (list arg) (list arg-sym))))

(defun single-call (spec name sym)
  (destructuring-bind ((arg) &body body) (rest (assoc name spec))
    (replace-symbols body (list arg) (list sym)))) 

(defun construct-context-call (spec)
  (destructuring-bind (() &body body) (rest (assoc :construct-context spec))
    body))

(defun buffer-call (spec name context-sym buffer-sym start-sym end-sym)
  (destructuring-bind ((ctx buf start end) &body body) (rest (assoc name spec))
    (replace-symbols body (list ctx buf start end) (list context-sym buffer-sym start-sym end-sym))))

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
