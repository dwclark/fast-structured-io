(in-package :fast-structured-io-ld)

(defmacro ld (name (parser-type context-type) &body spec)
  (with-unique-names (start current end evt read-buffer next-event)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (parser context)
	       (declare (optimize (speed 3) (safety 0)))
	       (declare (dynamic-extent parser))
	       (declare (type ,parser-type parser))
	       (declare (type ,context-type context))
	       (labels ((,next-event ()
			  (let ((,start (the fixnum ,(mixin-call spec :pos 'parser)))
				(,current (the character ,(mixin-call spec :current-char 'parser))))
			    (loop do (case ,current
				       (#\Return
					(let ((,end (the fixnum ,(mixin-call spec :pos 'parser))))
					  (if (char= #\Linefeed ,(mixin-call spec :next-char 'parser))
					      ,(mixin-call spec :advance 'parser))
					  (return (values :line ,start ,end))))
				       
				       ;; See https://docs.python.org/3/library/stdtypes.html#str.splitlines
				       ;; for behavior of python splitlines, which this emulates
				       ((#\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR)
					(let ((,end (the fixnum ,(mixin-call spec :pos 'parser))))
					  ,(mixin-call spec :advance 'parser)
					  (return (values :line ,start ,end))))
				       
				       (#\Nul
					(let ((,end (the fixnum ,(mixin-call spec :pos 'parser))))
					  (if (= ,start ,end)
					      (return (values :eof -1 -1))
					      (return (values :line ,start ,end)))))
				       
				       (otherwise (setf ,current ,(mixin-call spec :next-char 'parser))))))))
		 
		 (loop do (multiple-value-bind (,evt ,start ,end) (,next-event)
			    (declare (type symbol ,evt))
			    (declare (type fixnum ,start ,end))
			    (declare (ignorable ,start ,end))
			    
			    (case ,evt
			      (:line
			       (let ((,read-buffer ,(mixin-call spec :read-buffer 'parser)))
				 (declare (ignorable ,read-buffer))
				 (setf context ,(mixin-call spec :on-line 'context read-buffer start end))
				 ,(mixin-call spec :reset-buffer 'parser)))
			      (:eof
			       (return ,(mixin-call spec :on-eof 'context))))))))))))
  
(defmacro mixin (name parser-type parser-functions context-type context-functions)
  `(ld ,name (,parser-type ,context-type)
     ,@(funcall (symbol-function parser-functions))
     ,@(funcall (symbol-function context-functions))))

