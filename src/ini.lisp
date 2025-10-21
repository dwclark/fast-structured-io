(in-package :fast-structured-io-ini)

;; Syntax of ini to support
;; 1. k/v pairs in global area are supported, they are kept under the "" key unless
;; 2. they are the only set of k/v pairs, in which case they are converted to a single
;; map/alist and returned. Effectively, this means the file is a java properties file of sorts
;; 3. Sections are denoted by []. Whatever is between [] is the section name,
;; no semantic meaning is attached to the internals of the section. Any semantic
;; meaning must be done via post processing functions
;; 4. Comments start with # or ;. From those characters to the end of the line is a comment
;; 5. End of line is what is supported by ld as end of line
;; 6. k/v pairs are separated by either : or =. Keys may use escape char to include : or =.
;; 7. Values are whatever follows the = or : to the end of line.
;; 8. Escapes can only precede :, =, another escape, or end of line. Escape is \
;; 9. Quotes can enclose keys or values, embedded quotes can be escaped with escape. Quotes
;; are not included in k/v strings. Quote is "".
;; 10. Whitespace preceding or following k/v pairs is ignored.
;; 11. Whitespace is horizontal tab or space

(defmacro ini (name (parser-type context-type) &body spec)
  (with-unique-names (start current end evt read-buffer next-event whitespace-p consume-whitespace process-group
			    consume->eol move consume-quote process-key process-value)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (parser context)
	       (declare (optimize (speed 3) (safety 0)))
	       (declare (dynamic-extent parser))
	       (declare (type ,parser-type parser))
	       (declare (type ,context-type context))
	       (labels ((,move ()
			  (setf ,current ,(mixin-call spec :next-char 'parser)))
			
			(,whitespace-p () (or (char= #\Space ,current) (char= #\Tab ,current)))
			
			(,consume-whitespace ()
			  (loop while (,whitespace-p)
				do (,move)))
			
			(,consume->eol ()
			  (loop do (case ,current
				     (#\Return
				      (,move)
				      (when (char= #\Linefeed ,current)
					(,move)
					(return)))
				     
				     ((#\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul)
				      (,move)
				      (return))
				     
				     (otherwise
				      (,move)))))

			(,consume-quote (if-not-escaped if-escaped)
			  (,move)
			  (let* ((evt if-not-escaped)
				 (begin-str ,(mixin-call spec :pos 'parser)))
			    (loop do (case ,current
				       (#\"
					(return (values evt begin-str ,(mixin-call spec :pos 'parser))))
				       
				       (#\Nul
					(error "Got EOF before seeing "\"" char"))
				       
				       (#\\
					(setf evt if-escaped)
					(,move)
					(case ,current
					  ((#\\ #\")
					   (,move))

					  (otherwise
					   (error "only '\' and '\"' allowed to be escaped in quotes"))))
				       
				       (otherwise (,move))))))
			
			(,process-key ()
			  (,consume-whitespace)
			  (if (char= #\" ,current)
			      (multiple-value-bind (evt-type begin-str end-str) (,consume-quote :key :escaped-key)
				(,consume-whitespace)
				(,move)
				(case ,current
				  ((#\= #\:)
				   (values evt-type begin-str end-str))
				  (otherwise
				   (error "expected ':' or '='"))))
			      (let* ((evt-type :key)
				     (begin-str ,(mixin-call spec :pos 'parser))
				     (end-str begin-str))
				(loop do (,move)
					 (case ,current
					   ((#\: #\=)
					    (return (values evt-type begin-str (1+ end-str))))
					   
					   (#\\
					    (setf evt-type :escaped-key)
					    (,move)
					    (if (or (char= #\: ,current) (char= #\= ,current) (char= #\\ ,current))
						(setf end-str ,(mixin-call spec :pos 'parser))
						(error "can only escape ':', '=', or '\n in key")))
					    
					   ((#\# #\;)
					    (error "can't have '#' or ';' in key"))

					   (otherwise
					    (when (not (,whitespace-p))
					      (setf end-str ,(mixin-call spec :pos 'parser)))))))))

			(,process-value ()
			  (,consume-whitespace)
			  (if (char= #\" ,current)
			      (multiple-value-bind (evt-type begin-str end-str) (,consume-quote :value :escaped-value)
				(,consume->eol)
				(values evt-type begin-str end-str))
			      (let* ((evt-type :value)
				     (begin-str ,(mixin-call spec :pos 'parser))
				     (end-str begin-str))
				(loop do (,move)
					 (case ,current
					   ((#\# #\; #\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs
						 #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul)
					    (,consume->eol)
					    (return (values evt-type begin-str (1+ end-str))))
					   
					   (#\\
					    (setf evt-type :escaped-value)
					    (,move)
					    (case ,current
					      (#\Return
					       (,move)
					       (if (char= #\Linefeed ,current)
						   (,move)))
				     
					      ((#\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR)
					       (,move))

					      (#\\
					       (setf end-str ,(mixin-call spec :pos 'parser)))

					      (otherwise
					       (error "in value, '\' can only precede '\' or eol value"))))
					   
					   (otherwise
					    (if (not (,whitespace-p))
						(setf end-str ,(mixin-call spec :pos 'parser)))))))))
			
			(,process-group ()
			  (,consume-whitespace)
			  (let* ((begin-group ,(mixin-call spec :pos 'parser))
				 (end-group begin-group))
			    (loop while (not (char= #\] ,current))
				  do (if (not (,whitespace-p))
					 (setf end-group ,(mixin-call spec :pos 'parser)))
				     (,move))
			    (,consume->eol)
			    (values :group begin-group (1+ end-group))))
			
			(,next-event ()
			  (,consume-whitespace)
			  (case ,current
			    ((#\# #\; #\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs
				  #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR)
			     (,consume->eol)
			     (values :eol -1 -1))
			    
			    (#\Nul
			     (values :eof -1 -1))
			    
			    (#\[
			     (,move)
			     (,process-group))
			    
			    ((#\= #\:)
			     (,move)
			     (,process-value))
			    
			    (otherwise
			     (,process-key)))))

		 (setf ,current ,(mixin-call spec :current-char 'parser))
		 (loop do (multiple-value-bind (,evt ,start ,end) (,next-event)
			    (declare (type symbol ,evt))
			    (declare (type fixnum ,start ,end))
			    (declare (ignorable ,start ,end))
			    (let ((,read-buffer ,(mixin-call spec :read-buffer 'parser)))
			      (declare (ignorable ,read-buffer))
			      (case ,evt
				(:group
				 (setf context ,(mixin-call spec :on-group 'context read-buffer start end)))

				(:key
				 (setf context ,(mixin-call spec :on-key 'context read-buffer start end nil)))

				(:escaped-key
				 (setf context ,(mixin-call spec :on-key 'context read-buffer start end #\\)))
				
				(:value
				 (setf context ,(mixin-call spec :on-value 'context read-buffer start end nil)))

				(:escaped-value
				 (setf context ,(mixin-call spec :on-value 'context read-buffer start end #\\)))
				
				(:eol nil)
				
				(:eof (return ,(mixin-call spec :on-eof 'context))))
			      
			      ,(mixin-call spec :reset-buffer 'parser))))))))))

(defmacro mixin (name parser-type parser-functions context-type context-functions)
  `(ini ,name (,parser-type ,context-type)
     ,@(funcall (symbol-function parser-functions))
     ,@(funcall (symbol-function context-functions))))
