(in-package :fast-structured-io-properties)

(defmacro properties (name (parser-type context-type) &body spec)
  (with-unique-names (current start end evt read-buffer process-key process-value process-comment move
			      next-event skip-whitespace-key skip-whitespace-value skip-newline last-evt check-escpape)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (parser context)
	       ;;(declare (optimize (speed 3) (safety 0)))
	       (let* ((,last-evt (the symbol :value))
		      (,start (the fixnum 0))
		      (,current (the character ,(mixin-call spec :current-char 'parser))))

		 (declare (dynamic-extent parser))
		 (declare (type ,parser-type parser))
		 (declare (type ,context-type context))
		 (labels ((,move () (setf ,current ,(mixin-call spec :next-char 'parser)))

			  (,skip-newline ()
			    (,move)
			    (loop while (or (char= #\Newline ,current) (char= #\Return ,current))
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
				       (otherwise (return)))))

			  (,skip-whitespace-value ()
			    (loop do (case ,current
				       ((#\Space #\Tab #\Formfeed) (,move))
				       (otherwise (return)))))

			  (,process-key ()
			    (let ((,start (the fixnum ,(mixin-call spec :pos 'parser)))
				  (evt-type :key))
			      (loop do (case ,current
					 (#\\
					  (setf evt-type :escaped-key)
					  (,move)
					  (case ,current
					    ((#\Newline #\Return) (,skip-newline))
					    (#\u (,check-escape))
					    (otherwise (,move))))
					 
					 ((#\Space #\Tab #\Formfeed #\: #\=)
					  (return (values :evt-type ,start ,(mixin-call spec :pos 'parser))))

					 (otherwise (,move))))))
			  
			  (,process-value ()
			    (,skip-whitespace-value)
			    (let ((,start (the fixnum ,(mixin-call spec :pos 'parser)))
				  (evt-type :value))
			      (loop do (case ,current
					 (#\\
					  (setf evt-type :escaped-value)
					  (,move)
					  ((#\Newline #\Return)
					   (,skip-newline)
					   (,skip-whitespace-value))
					  (#\u (,check-escape))
					  (otherwise (,move)))

					 ((#\Newline #\Return)
					  (let ((evt-end (the fixnum ,(mixin-call spec :pos 'parser))))
					    (,skip-newline)
					    (return (values :evt-type ,start evt-end))))

					 (otherwise (,move))))))

			  (,next-event ()
			    (case ,last-evt
			      ((:value :escaped-value)
			       (,skip-whitespace-key)
			       (,process-key))
			      ((:key :escaped-key)
			       (,move)
			       (,process-value)))))
		   
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
				 (return ,(mixin-call spec :on-eof 'context)))))))))))))
