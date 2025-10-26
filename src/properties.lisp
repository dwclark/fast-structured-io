(in-package :fast-structured-io-properties)

(defmacro properties (name (parser-type context-type) &body spec)
  (with-unique-names (current read-buffer process-key process-value skip-past-eol move handle-escapes
			      next-event skip-whitespace-key skip-whitespace-value skip-newline check-escape
			      evt key-escape key-start key-end value-escape value-start value-end)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (parser context)
	       (declare (optimize (speed 3) (safety 0)))
	       (let* ((,current (the character ,(mixin-call spec :current-char 'parser)))
		      (,key-escape nil)
		      (,key-start 0)
		      (,key-end 0)
		      (,value-escape nil)
		      (,value-start 0)
		      (,value-end 0))
		 
		 (declare (dynamic-extent parser))
		 (declare (type ,parser-type parser))
		 (declare (type ,context-type context))
		 (declare (type boolean ,key-escape ,value-escape))
		 (declare (type fixnum ,key-start ,key-end ,value-start ,value-end))
		 (declare (inline ,move ,skip-newline ,check-escape ,skip-past-eol ,skip-whitespace-key
				  ,skip-whitespace-value ,handle-escapes ,process-key ,process-value))
		 
		 (labels ((,move ()
			    (setf ,current ,(mixin-call spec :next-char 'parser)))
			  
			  (,skip-newline ()
			    (loop while (eol-p ,current)
				  do (,move)))
			  
			  (,check-escape ()
			    (dotimes (i 4)
			      (declare (ignore i))
			      (if (not (digit-char-p ,current 16))
				  (error "invalid unicode escape sequence"))
			      (,move)))

			  (,skip-past-eol ()
			    (loop while (not (eol-p ,current))
				  do (,move))
			    (,skip-newline))

			  (,skip-whitespace-key ()
			    (loop do (case ,current
				       ((#\Space #\Tab #\Formfeed #\Newline #\Return)
					(,move))
				       ((#\# #\!)
					(,move)
					(,skip-past-eol))
				       (otherwise (return)))))
			  
			  (,skip-whitespace-value ()
			    (loop do (case ,current
				       ((#\Space #\Tab #\Formfeed) (,move))
				       (otherwise (return)))))

			  (,handle-escapes ()
			    (,move)
			    (case ,current
			      ((#\Newline #\Return)
			       (,skip-newline)
			       (,skip-whitespace-value))
			      (#\u
			       (,move)
			       (,check-escape))
			      (otherwise
			       (,move))))

			  (,process-key ()
			    (setf ,key-start ,(mixin-call spec :pos 'parser)
				  ,key-escape nil)
			    (loop do (case ,current
				       (#\\
					(setf ,key-escape t)
					(,handle-escapes))
				       
				       ((#\Space #\Tab #\Formfeed #\: #\=)
					(setf ,key-end ,(mixin-call spec :pos 'parser))
					(return))
				       
				       (#\Nul (error "found eof while parsing key"))
				       
				       (otherwise (,move)))))
			  
			  (,process-value ()
			    (setf ,value-start ,(mixin-call spec :pos 'parser)
				  ,value-escape nil)
			    (loop do (case ,current
				       (#\\
					(setf ,value-escape t)
					(,handle-escapes))
				       
				       ((#\Newline #\Return)
					(setf ,value-end ,(mixin-call spec :pos 'parser))
					(,skip-newline)
					(return))
				       
				       (#\Nul
					(setf ,value-end ,(mixin-call spec :pos 'parser))
					(return))
				       
				       (otherwise
					(,move)))))
			  
			  (,next-event ()
			    (,skip-whitespace-key)
			    (if (char= #\Nul ,current)
				(return-from ,next-event :eof))
			    (,process-key)
			    (,move)
			    (,skip-whitespace-value)
			    (if (char= #\Nul ,current)
				(error "found eof before finding value"))
			    (,process-value)
			    :on-line))
		   
		   (loop do (let ((,evt (the symbol (,next-event))))
			      (if (eq ,evt :eof)
				  (return ,(mixin-call spec :on-eof 'context))
				  (let ((,read-buffer ,(mixin-call spec :read-buffer 'parser)))
				    (declare (ignorable ,read-buffer))
				    (setf context ,(mixin-call spec :on-line 'context read-buffer
							       key-start key-end key-escape
							       value-start value-end value-escape))
				    ,(mixin-call spec :reset-buffer 'parser))))))))))))

(defmacro mixin (name parser-type parser-functions context-type context-functions)
  `(properties ,name (,parser-type ,context-type)
     ,@(funcall (symbol-function parser-functions))
     ,@(funcall (symbol-function context-functions))))
