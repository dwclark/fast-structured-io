(in-package :fast-structured-io-utils)

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

(defun str-functions ()
  '((:pos (parser) (str-parser-pos parser))
    (:advance (parser) (str-parser-advance parser))
    (:read-buffer (parser) (str-parser-read-buffer parser))
    (:reset-buffer (parser) nil)
    (:next-char (parser) (str-parser-next parser))
    (:current-char (parser) (str-parser-char parser))))

(declaim (inline stm-parser-read-buffer stm-parser-pos stm-parser-current stm-parser-move-next stm-parser-reset))

(defstruct stm-parser
  (read-buffer (make-string 512) :type simple-string)
  (pos -1 :type fixnum)
  (stm nil :type stream))

(defun stm-parser-current (parser)
  (declare (type stm-parser parser))
  (schar (stm-parser-read-buffer parser) (stm-parser-pos parser)))

(defun stm-resize-read-buffer (parser)
  (declare (type stm-parser parser))
  (loop with old of-type simple-string = (stm-parser-read-buffer parser)
	with new of-type simple-string = (make-string (+ 512 (array-total-size old)))
	for i from 0 below (array-total-size old)
	do (setf (schar new i) (schar old i))
	finally (setf (stm-parser-read-buffer parser) new)))

(defun stm-parser-move-next (parser)
  (declare (type stm-parser parser))
  (when (= (1+ (stm-parser-pos parser)) (array-total-size (stm-parser-read-buffer parser)))
    (stm-resize-read-buffer parser))
  
  (incf (stm-parser-pos parser))
  (setf (schar (stm-parser-read-buffer parser) (stm-parser-pos parser))
	(read-char (stm-parser-stm parser) nil #\Nul)))
  
(defun stm-parser-reset (parser)
  (declare (type stm-parser parser))
  (setf (schar (stm-parser-read-buffer parser) 0) (stm-parser-current parser))
  (setf (stm-parser-pos parser) 0))

(defun stm-parser-new (stm)
  (let ((tmp (make-stm-parser :stm stm)))
    (stm-parser-move-next tmp)
    tmp))

(defun stm-functions ()
  '((:pos (parser) (stm-parser-pos parser))
    (:advance (parser) (stm-parser-move-next parser))
    (:read-buffer (parser) (stm-parser-read-buffer parser))
    (:reset-buffer (parser) (stm-parser-reset parser))
    (:next-char (parser) (stm-parser-move-next parser))
    (:current-char (parser) (stm-parser-current parser))))
