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

(defmacro ld-str (name &key (context-type t) construct-context on-line (on-eof 'identity))
  `(line-delimited ,name
		   :parser-type str-parser :construct-parser str-parser-construct
		   :pos str-parser-pos :advance str-parser-advance
		   :read-buffer str-parser-read-buffer :reset-buffer call->nil
		   :next-char str-parser-next :current-char str-parser-char
		   :context-type ,context-type :construct-context ,construct-context :on-line ,on-line :on-eof ,on-eof))

(ld-str ld-str-noop
	:context-type list :construct-context (call->nil) :on-line call->nil :on-eof call->nil)

(defmacro increment-lines (lines buffer start end)
  `(incf ,lines))

(ld-str ld-str-count-lines
	:context-type fixnum
	:construct-context (call->zero)
	:on-line increment-lines)

(declaim (inline accum-list-strings accum-list-ints accum-vec-ints))

(defun accum-list-strings (cell buffer start end)
  (accum-list cell (subseq buffer start end)))

(defun accum-list-ints (cell buffer start end)
  (accum-list cell (parse-integer buffer :start start :end end)))

(defun accum-vec-ints (vec buffer start end)
  (vector-push-extend (parse-integer buffer :start start :end end) vec)
  vec)

(ld-str ld-str->list
	:context-type cons
	:construct-context (accum-list-init)
	:on-line accum-list-strings
	:on-eof accum-list-extract)

(ld-str ld-str->list-ints
	:context-type cons
	:construct-context (accum-list-init)
	:on-line accum-list-ints
	:on-eof accum-list-extract)

(ld-str ld-str->vec-ints
	:context-type (vector fixnum *)
	:construct-context (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)
	:on-line accum-vec-ints)
