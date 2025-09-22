(in-package :fast-structured-io)

(defmacro line-delimited
    (function-name &key parser-type construct-parser pos advance read-buffer reset-buffer next-char current-char
		     context-type construct-context on-line on-eof)
  (let ((func-name (symbol-name function-name)))
    `(setf (symbol-function (intern ,func-name))
	   (lambda (arg)
	     (declare (optimize (speed 3) (safety 0)))
	     (let ((parser (,construct-parser arg))
		   (context ,construct-context))
	       (declare (dynamic-extent parser))
	       (declare (type ,parser-type parser))
	       (declare (type ,context-type context))
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

			    (case evt
			      (:line
			       (setf context (,on-line context (,read-buffer parser) start end))
			       (,reset-buffer parser))
			      (:eof
			       (return (,on-eof context))))))))))))

(defmacro line-delimited-str (name &key context-type construct-context on-line on-eof)
  `(line-delimited ,name
		   :parser-type str-parser :construct-parser str-parser-construct
		   :pos str-parser-pos :advance str-parser-advance
		   :read-buffer str-parser-read-buffer :reset-buffer macro->nil
		   :next-char str-parser-next :current-char str-parser-char
		   :context-type ,context-type :construct-context ,construct-context :on-line ,on-line :on-eof ,on-eof))

(line-delimited-str line-delimited-str-noop
		    :context-type list :construct-context `nil :on-line macro->nil :on-eof macro->nil)

(defmacro increment-lines (lines buffer start end)
  `(incf ,lines))

(line-delimited-str str-count-lines
		    :context-type fixnum :construct-context 0
		    :on-line increment-lines :on-eof identity)

(defmacro line-delimited-str->list (name &key (extract 'subseq))
  (let* ((cell (gensym))
	 (buffer (gensym))
	 (start (gensym))
	 (end (gensym))
	 (on-line-lambda `(lambda (,cell ,buffer ,start ,end)
			    (let ((next-cell (cons (,extract ,buffer ,start ,end) nil)))
			      (cond
				((cdr ,cell)
				 (setf (cdr (cdr ,cell)) next-cell)
				 (setf (cdr ,cell) next-cell))
				(t
				 (setf (car ,cell) next-cell)
				 (setf (cdr ,cell) next-cell))))
			    ,cell)))
    
    `(line-delimited-str ,name
			 :context-type cons
			 :construct-context (cons nil nil)
			 :on-line ,on-line-lambda :on-eof car)))
    
(line-delimited-str->list line-delimited->str-list)

(defmacro str->int (buf start end)
  `(parse-integer ,buf :start ,start :end ,end))

(line-delimited-str->list line-delimited->int-list :extract str->int)
;; to make an int list:
;; (defun str->int (str start end)
;;	(parse-integer str :start start :end end))
;; (line-delimited-str->list line-delimited->int-list :extract str->int)
