p(in-package :fast-structured-io-properties)

(defmacro properties (name (parser-type context-type) &body spec)
  (with-unique-names (current read-buffer process-key process-value process-comment move
			      next-event skip-whitespace-key skip-whitespace-value skip-newline check-escpape
			      key-evt key-start key-end value-evt value-start value-end)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (parser context)
	       ;;(declare (optimize (speed 3) (safety 0)))
	       (let* ((,current (the character ,(mixin-call spec :current-char 'parser)))
		      (,key-evt :none)
		      (,key-start 0)
		      (,key-end 0)
		      (,value-evt :none)
		      (,value-start 0)
		      (,value-end 0))
		 
		 (declare (dynamic-extent parser))
		 (declare (type ,parser-type parser))
		 (declare (type ,context-type context))
		 (declare (type symbol ,key-evt ,value-evt))
		 (declare (type fixnum ,key-start ,key-end ,value-start ,value-end))
		 
		 (labels ((,move () (setf ,current ,(mixin-call spec :next-char 'parser)))
			  
			  (,skip-newline ()
			    (,move)
			    (loop while (eol-p ,current)
				  do (,move)))

			  (,check-escape ()
			    (dotimes (i 4)
			      (ignore i)
			      (,move)
			      (if (not (digit-char-p ,current 16))
				  (error "invalid unicode escape sequence"))))

			  (,process-comment ()
			    (loop do (,move)
				     (case ,current
				       ((#\Newline #\Return)
					(,skip-newline)
					(return))
				       (otherwise (,move)))))

			  (,skip-whitespace-key ()
			    (loop do (case ,current
				       ((#\Space #\Tab #\Formfeed #\Newline #\Return) (,move))
				       ((#\# #\!) (,process-comment))
				       (#\Nul (setf ,key-evt :eof) (return))
				       (otherwise (return)))))

			  (,skip-whitespace-value ()
			    (loop do (case ,current
				       ((#\Space #\Tab #\Formfeed) (,move))
				       (otherwise (return)))))

			  (,process-key ()
			    (setf ,key-start ,(mixin-call spec :pos 'parser)
				  ,key-evt :key)
			    (loop do (case ,current
				       (#\\
					(setf ,key-evt :escaped-key)
					(,move)
					(case ,current
					  ((#\Newline #\Return) (,skip-newline))
					  (#\u (,check-escape))
					  (otherwise (,move))))
				       
				       ((#\Space #\Tab #\Formfeed #\: #\=)
					(setf ,key-end ,(mixin-call spec :pos 'parser))
					(return))
					 
				       (otherwise (,move)))))
			  
			  (,process-value ()
			    (,skip-whitespace-value)
			    (setf ,value-start ,(mixin-call spec :pos 'parser)
				  ,value-evt :value)
			    (loop do (case ,current
				       (#\\
					(setf ,value-evt :escaped-value)
					(,move)
					((#\Newline #\Return)
					 (,skip-newline)
					 (,skip-whitespace-value))
					(#\u (,check-escape))
					(otherwise (,move)))
				       
				       ((#\Newline #\Return)
					(setf ,value-end ,(mixin-call spec :pos 'parser))
					(,skip-newline)
					(return))
				       
				       (otherwise (,move)))))
			  
			  (,next-event ()
			    ;;Should only eof in skip-whitespace key
			    ;;or at the end of process-values
			    ;;anywhere else will be an error
			    (,skip-whitespace-key)
			    (if (eq :eof ,key-type)
				(return-from ,next-event))
			    
			    (,process-key)
			    (,move)
			    (,procees-values)))
		   
		   (loop do (,next-event)
			    (cond ((eq ,value-evt :eof)
				   (error "found eol while looking for value"))

				  ((eq ,key-evt :eof)
				   (return ,(mixin-call spec :on-eof 'context)))

				  (t
				   (let ((,read-buffer ,(mixin-call spec :read-buffer 'parser)))
				     (declare (ignorable ,read-buffer))
				     (ecase ,key-evt
				       (:key
					(setf context ,(mixin-call spec :on-key 'context read-buffer start end nil)))
				       (:escaped-key
					(setf context ,(mixin-call spec :on-key 'context read-buffer start end t))))
				     (ecase ,value-evt
				       (:value
					(setf context ,(mixin-call spec :on-value 'context read-buffer start end nil)))
				       (:escaped-value
					(setf context ,(mixin-call spec :on-value 'context read-buffer start end t))))
				     ,(mixin-call spec :reset-buffer 'parser))))))))))))
