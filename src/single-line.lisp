(in-package :fast-structured-io)

(defun construct (fn) (funcall fn :construct :init 'arg))
(defun pos (fn) (funcall fn :pos :ctx 'parser))
(defun advance (fn) (funcall fn :advance :ctx 'parser))
(defun read-buffer (fn) (funcall fn :read-buffer :ctx 'parser))
(defun reset-buffer (fn) (funcall fn :reset-buffer :ctx 'parser))
(defun next-char (fn) (funcall fn :next-char :ctx 'parser))
(defun current-char (fn) (funcall fn :current-char :ctx 'parser))
  
(defmacro define-single-line (function-name fn-expr)
  (let ((func-name (symbol-name function-name)))
    `(setf (symbol-function (intern ,func-name))
	   (lambda (arg)
	     (let ((parser ,(construct fn-expr)))
	       (declare (dynamic-extent parser))
	       (declare (type ,(funcall fn-expr :type) parser))
	       (labels ((next-event ()
			  (let* ((start (the fixnum ,(pos fn-expr)))
				 (current (the character ,(current-char fn-expr))))
			    (loop do (case current
				       (#\Return
					(let ((end (the fixnum ,(pos fn-expr))))
					  (if (char= #\Linefeed ,(next-char fn-expr))
					      ,(advance fn-expr))
					  (return (values :line start end))))

				       (#\Linefeed
					(let ((end (the fixnum ,(pos fn-expr))))
					  ,(advance fn-expr)
					  (return (values :line start end))))

				       (#\Nul
					(return (values :eof -1 -1)))
				       
				       (otherwise (setf current ,(next-char fn-expr))))))))
		 
		 (loop do (multiple-value-bind (evt start end) (next-event)
			    (declare (type symbol evt))
			    (declare (type fixnum start end))

			    (ecase evt
			      (:line (format t "line start: ~A end: ~A~%" start end))
			      (:eof
			       (format t "eof")
			       (return)))))))))))

(defmacro define-single-line-2
    (function-name &key parser-type construct pos advance read-buffer reset-buffer next-char current-char
		     on-line on-eof)
  (let ((func-name (symbol-name function-name)))
    `(setf (symbol-function (intern ,func-name))
	   (lambda (arg)
	     (let ((parser (,construct arg)))
	       (declare (dynamic-extent parser))
	       (declare (type ,parser-type parser))
	       (labels ((next-event ()
			  (let* ((start (the fixnum (,pos parser)))
				 (current (the character (,current-char parser))))
			    (loop do (case current
				       (#\Return
					(let ((end (the fixnum (,pos parser))))
					  (if (char= #\Linefeed (,next-char parser))
					      (,advance parser))
					  (return (values :line start end))))

				       (#\Linefeed
					(let ((end (the fixnum (,pos parser))))
					  (,advance parser)
					  (return (values :line start end))))

				       (#\Nul
					(let ((end (the fixnum (,pos parser))))
					  (if (= start end)
					      (return (values :eof -1 -1))
					      (return (values :line start end)))))
				       
				       (otherwise (setf current (,next-char parser))))))))
		 
		 (loop do (multiple-value-bind (evt start end) (next-event)
			    (declare (type symbol evt))
			    (declare (type fixnum start end))

			    (ecase evt
			      (:line (,on-line (,read-buffer parser) start end))
			      (:eof (,on-eof)
			       (return)))))))))))

(defun on-line-nop (buffer start end)
  (format t "found ~A~%" (subseq buffer start end)))

(defun on-eof-nop ()
  (format t "at end"))

(define-single-line-2 simplest-2
  :parser-type str-parser :construct str-parser-construct-expr
  :pos str-parser-pos-expr :advance str-parser-advance-expr
  :read-buffer str-parser-read-buffer-expr :reset-buffer str-parser-reset-buffer-expr
  :next-char str-parser-next-char :current-char str-parser-current-char
  :on-line on-line-nop :on-eof on-eof-nop)
