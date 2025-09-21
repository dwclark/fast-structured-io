(in-package :fast-structured-io)

(declaim (inline str-parser-read-buffer str-parser-pos str-parser-advance str-parser-char str-parser-current str-parser-next))

(defstruct str-parser
  (read-buffer "" :type simple-string)
  (pos 0 :type fixnum))

(defun str-parser-advance (parser)
  (declare (type str-parser parser))
  (incf (str-parser-pos parser)))

(defun str-parser-char (parser &optional (at (str-parser-pos parser)))
  (declare (type str-parser parser))
  (if (< at (array-total-size (str-parser-read-buffer parser)))
      (schar (str-parser-read-buffer parser) at)
      #\Nul))
  
(defun str-parser-next (parser)
  (declare (type str-parser parser))
  (str-parser-char parser (str-parser-advance parser)))

(defun str-parser-expression (expr-type &key init ctx)
  (flet ((sub (expr)
	   (substitute ctx :ctx (substitute init :init expr))))
    (case expr-type
      (:type 'str-parser)
      (:construct (sub '(make-str-parser :read-buffer :init :pos 0)))
      (:pos (sub '(str-parser-pos :ctx)))
      (:advance (sub '(str-parser-advance :ctx)))
      (:read-buffer (sub '(str-parser-read-buffer :ctx)))
      (:reset-buffer (sub 'nil))
      (:next-char (sub '(str-parser-next :ctx)))
      (:current-char (sub '(str-parser-char :ctx))))))
    
(defmacro str-parser-construct-expr (x)
  `(make-str-parser :read-buffer ,x :pos 0))

(defmacro str-parser-pos-expr (x)
  `(str-parser-pos ,x))

(defmacro str-parser-advance-expr (x)
  `(str-parser-advance ,x))

(defmacro str-parser-read-buffer-expr (x)
  `(str-parser-read-buffer ,x))

(defmacro str-parser-reset-buffer-expr (x) 'nil)

(defmacro str-parser-next-char (x)
  `(str-parser-next ,x))

(defmacro str-parser-current-char (x) 
  `(str-parser-char ,x))
