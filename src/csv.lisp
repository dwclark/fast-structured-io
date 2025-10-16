(in-package :fast-structured-io-csv)

;;TODO, there's a mistake in how there's a difference in the parser and the context
;;with the parser, we always allow an in parameter to be passed to the parser "constructor"
;;Note how we don't allow any parameters/information for the context "constructor"
;;this makes it impossible to have anything but a default constructor for the context.
;;while this is possible because we can always write a specialized function for the constructor
;;it also means we have to do a completely new mixin.
;;need to do one of two things:
;;1. provide a context constructor argument or arguments to allow specifying how
;;the context constructor should be created
;;2. Don't provide hooks to call the constructor, require two arguments, one for the
;;complete parser, the other for the complete context
;;I'm leaning towards 2.
(defmacro csv (name (in) &body spec)
  (with-unique-names (parser context start current end evt evt-start evt-end evt-type read-buffer next-event unquoted quoted)
    (let ((func-name (symbol-name name))
	  (separator-char (mixin-or-default spec :separator-char #\,))
	  (quote-char (mixin-or-default spec :quote-char #\"))
	  (escape-char (mixin-or-default spec :escape-char #\")))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (,in)
	       ;;(declare (optimize (speed 3) (safety 0)))
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

		 (labels ((,unquoted ()
			    (setf ,current ,(mixin-call spec :next-char parser))
			    (loop do
			      (case ,current
				(,separator-char
				 (let ((,end ,(mixin-call spec :pos parser)))
				   (setf ,current ,(mixin-call spec :next-char parser))
				   (return (values :field ,start ,end))))

				((#\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul)
				 (return (values :field ,start ,(mixin-call spec :pos parser))))

				(otherwise
				 (setf ,current ,(mixin-call spec :next-char parser))))))

			  (,quoted ()
			    (setf ,current ,(mixin-call spec :next-char parser))
			    (setf ,start ,(mixin-call spec :pos parser))
			    (let ((,evt-type (the symbol :field)))
			      (if (char-equal ,quote-char ,escape-char)
				  ;;quote-char and escape-char are the same
				  (loop do
				    (case ,current
				      (,quote-char
				       (setf ,current ,(mixin-call spec :next-char parser))
				       (case ,current
					 (,quote-char
					  (setf ,evt-type :escaped-field)
					  (setf ,current ,(mixin-call spec :next-char parser)))
					 
					 (,separator-char
					  (let ((,end (1- ,(mixin-call spec :pos parser))))
					    (setf ,current ,(mixin-call spec :next-char parser))
					    (return (values ,evt-type ,start ,end))))

					 ((#\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul)
					  (return (values ,evt-type ,start (1- ,(mixin-call spec :pos parser)))))))

				      (t
				       (setf ,current ,(mixin-call spec :next-char parser)))))

				  ;; quote-char and escape-char are different
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
				 (setf ,current ,(mixin-call spec :next-char parser))
				 (case ,current
				   ((#\Return #\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR #\Nul)
				    (return (values :empty-field 2 2)))
				   (otherwise
				    (return (values :empty-field 1 1)))))
				
				(#\Return
				 (let ((,end ,(mixin-call spec :pos parser)))
				   (if (char= #\Linefeed ,(mixin-call spec :next-char parser))
				       ,(mixin-call spec :advance parser))
				   (return (values :line -1 -1))))
				
				;; See https://docs.python.org/3/library/stdtypes.html#str.splitlines
				;; for behavior of python splitlines, which this emulates
				((#\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR)
				 (let ((,end ,(mixin-call spec :pos parser)))
				   ,(mixin-call spec :advance parser)
				   (return (values :line -1 -1))))
				
				(#\Nul
				 (return (values :eof -1 -1)))

				(,quote-char
				 (return (,quoted)))
				
				(otherwise
				 (return (,unquoted)))))))

		   (loop do (multiple-value-bind (,evt ,evt-start ,evt-end) (,next-event)
			      (declare (type symbol ,evt))
			      (declare (type fixnum ,evt-start ,evt-end))
			      (declare (ignorable ,evt-start ,evt-end))

			      (case ,evt
				(:empty-field
				 (setf ,context ,(mixin-call spec :on-empty-field context))
				 (if (= 2 ,evt-start)
				     (setf ,context ,(mixin-call spec :on-empty-field context))))
				
				(:field
				 (let ((,read-buffer ,(mixin-call spec :read-buffer parser)))
				   (declare (ignorable ,read-buffer))
				   (setf ,context ,(mixin-call spec :on-field context read-buffer evt-start evt-end))
				   ,(mixin-call spec :reset-buffer parser)))

				(:escaped-field
				 (let ((,read-buffer ,(mixin-call spec :read-buffer parser)))
				   (declare (ignorable ,read-buffer))
				   (setf ,context ,(mixin-call spec :on-escaped-field context read-buffer evt-start evt-end escape-char))
				   ,(mixin-call spec :reset-buffer parser)))

				(:line
				 (setf ,context ,(mixin-call spec :on-line context)))

				(:eof
				 (return ,(mixin-call spec :on-eof context)))))))))))))

(defun mixer (sep quot esc parser-impl event-impl)
  (append (chars sep quot esc) parser-impl event-impl))
	  
(defmacro mixin (name in sep quot esc parser-functions event-functions)
  (let* ((eval-parser (eval parser-functions))
	 (eval-event (eval event-functions))
	 (cs (chars `,sep `,quot `,esc))
	 (mixed (append eval-parser eval-event cs)))
    (format t "~A~%" mixed)
    `(csv ,name (,in)
       ,mixed)))


