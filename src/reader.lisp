(in-package :fast-structured-io)

(declaim (inline str-parser-read-buffer str-parser-pos str-parser-advance str-parser-char str-parser-next))

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

