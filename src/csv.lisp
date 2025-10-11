(in-package :fast-structured-io)

(defmacro csv (name (in) &body spec)
  (with-unique-names (parser context start current end evt evt-start evt-end evt-type read-buffer next-event unquoted quoted
			     eol-char-p separator-char-p quote-char-p)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (,in &key (separator-char #\,) (quote-char #\") (escape-char #\"))
	       (declare (type character separator-char quote-char escape-char))
	       (declare (optimize (speed 3) (safety 0)))
	       (let* ((,parser ,(mixin-call spec :construct-parser in))
		      (,context ,(mixin-call spec :construct-context))
		      (,start 0)
		      (,current #\Nul))
		 (declare (ignorable ,context))
		 (declare (dynamic-extent ,parser))
		 (declare (type ,(mixin-type spec :parser-type) ,parser))
		 (declare (type ,(mixin-type spec :context-type) ,context))
		 (declare (type ,start fixnum))
		 (declare (type ,current character))
		 (declare (inline ,separator-char-p ,eol-char-p ,quote-char-p))
		 
		 (labels ((,separator-char-p () (char-equal separator-char ,current))

			  (,quote-char-p () (char-equal quote-char ,current))
			  
			  (,eol-char-p ()
			    (case ,current
			      ((#\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul) t)
			      (otherwise nil)))
			  
			  (,unquoted ()
			    (setf ,current ,(mixin-call spec :next-char parser))
			    (loop do
			      (cond ((,separator-char-p)
				     (let ((,end ,(mixin-call spec :pos parser)))
				       (setf ,current ,(mixin-call spec :next-char parser))
				       (return (values :field ,start ,end))))

				    ((,eol-char-p)
				     (return (values :field ,start ,(mixin-call spec :pos parser))))

				    (t
				     (setf ,current ,(mixin-call spec :next-char parser))))))

			  (,quoted ()
			    (setf ,current ,(mixin-call spec :next-char parser))
			    (setf ,start ,(mixin-call spec :pos parser))
			    (let ((,evt-type (the symbol :field)))
			      (if (eq quote-char escape-char)
				  ;;quote-char and escape-char are the same
				  (loop do
				    (cond ((,quote-char-p)
					   (setf ,current ,(mixin-call spec :next-char parser))
					   (cond ((,quote-char-p)
						  (setf ,evt-type :escaped-field)
						  (setf ,current ,(mixin-call spec :next-char parser)))
					 
						 ((,separator-char-p)
						  (let ((,end (1- ,(mixin-call spec :pos parser))))
						    (setf ,current ,(mixin-call spec :next-char))
						    (return (values ,evt-type ,start ,end))))

						 ((,eol-char-p)
					 	  (return (values ,evt-type ,start (1- ,(mixin-call spec :pos parser)))))))

					  (t
					   (setf ,current ,(mixin-call spec :next-char parser)))))

				  ;; quote-char and escape-char are different
				  ;; TODO: start here. Need to convert case -> cond
				  ;; separator-char, quote-char, and escape-char are no longer macro variables
				  ;; they are function variables and can change between invocations
				  (loop do
				    (case ,current
				      (,escape-char
				       (setf ,evt-type :escaped-field)
				       ,(mixin-call spec :advance parser)
				       (setf ,current ,(mixin-call spec :next-char parser)))
				      
				      (,quote-char
				       (let ((,end ,(mixin-call spec :pos parser)))
					 (setf ,current ,(mixin-call spec :next-char parser))
					 (ecase ,current
					   (,separator-char
					    (setf ,current ,(mixin-call spec :next-char parser))
					    (return (values ,evt-type ,start ,end)))

					   ((#\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul)
					    (return (values ,evt-type ,start ,end))))))

				      (otherwise
				       (setf ,current ,(mixin-call spec :next-char parser))))))))
			  
			  (,next-event ()
			    (setf ,start ,(mixin-call spec :pos parser))
			    (setf ,current ,(mixin-call spec :current-char parser))
			    
			    (loop do
			      (case ,current
				
				(,separator-char
				 ,(mixin-call spec :advance parser)
				 (return (values :field ,start ,start)))
				
				(#\Return
				 (let ((,end (the fixnum ,(mixin-call spec :pos parser))))
				   (if (char= #\Linefeed ,(mixin-call spec :next-char parser))
				       ,(mixin-call spec :advance parser))
				   (return (values :line -1 -1))))
				
				;; See https://docs.python.org/3/library/stdtypes.html#str.splitlines
				;; for behavior of python splitlines, which this emulates
				((#\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR)
				 (let ((,end (the fixnum ,(mixin-call spec :pos parser))))
				   ,(mixin-call spec :advance parser)
				   (return (values :line -1 -1))))
				
				(#\Nul
				 (let ((,end (the fixnum ,(mixin-call spec :pos parser))))
				   (if (= ,start ,end)
				       (return (values :eof -1 -1))
				       (return (values :line -1 -1)))))

				(if (not (char-equal #\Nul ,quote-char))
				    (,quote-char
				     (return (,quoted))))
				
				(otherwise
				 (return (,unquoted)))))))
		   
		   (loop do (multiple-value-bind (,evt ,evt-start ,evt-end) (,next-event)
			      (declare (type symbol ,evt))
			      (declare (type fixnum ,evt-start ,evt-end))
			      (declare (ignorable ,evt-start ,evt-end))
			      
			      (case ,evt
				(:field
				 (let ((,read-buffer ,(mixin-call spec :read-buffer parser)))
				   (declare (ignorable ,read-buffer))
				   (setf ,context ,(mixin-call spec :on-field context read-buffer evt-start evt-end))
				   ,(mixin-call spec :reset-buffer parser)))
				(:escaped-field
				 (let ((,read-buffer ,(mixin-call spec :read-buffer parser)))
				   (declare (ignorable ,read-buffer))
				   (setf ,context ,(mixin-call spec :on-escaped-field context read-buffer evt-start evt-end))
				   ,(mixin-call spec :reset-buffer parser)))
				(:line
				 (setf ,context ,(mixin-call spec :on-line context)))
				(:eof
				 (return ,(mixin-call spec :on-eof context)))))))))))))

#|(defun vec-accum-functions ()
  '((:construct-context () (make-array 1 :adjustable t :fill-pointer t
					 :initial-contents (list (make-array 0 :adjustable t :fill-pointer t))))
    (:on-field (vec buf start end) (vector-push-extend (subseq buf start end) (aref vec (1- (array-dimension vec 0)))))
    (:on-escaped-field (vec buf start end) (vector-push-extend (subseq buf start end) (aref vec (1- (array-dimension vec 0)))))
    (:on-line (vec) (vector-push-extend (make-array 0 :adjustable t :fill-pointer t) vec))
    (:on-eof (vec) vec)))

(defmacro csv-mixin (name in parser-functions event-functions &key (separator-char #\,) (quote-char #\Nul) (escape-char #\Nul))
  `(csv ,name (,in :separator-char ,separator-char :quote-char ,quote-char :escape-char ,escape-char)
     ,@(funcall (symbol-function parser-functions))
     ,@(funcall (symbol-function event-functions))))

(csv-mixin simple-csv->matrix in str-functions vec-accum-functions)
  |#   
				  
