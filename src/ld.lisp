(in-package :fast-structured-io)

(defun construct-parser-call (spec arg-sym)
  (destructuring-bind ((arg) &body body) (rest (assoc :construct-parser spec))
    (replace-symbols body (list arg) (list arg-sym))))

(defun single-call (spec name sym)
  (destructuring-bind ((arg) &body body) (rest (assoc name spec))
    (replace-symbols body (list arg) (list sym)))) 

(defun construct-context-call (spec)
  (destructuring-bind (() &body body) (rest (assoc :construct-context spec))
    body))

(defun buffer-call (spec name context-sym buffer-sym start-sym end-sym)
  (destructuring-bind ((ctx buf start end) &body body) (rest (assoc name spec))
    (replace-symbols body (list ctx buf start end) (list context-sym buffer-sym start-sym end-sym))))

(defmacro ld-2 (name (&key (parser-type t) (context-type t)) &body spec)
  (let ((func-name (symbol-name name)))
    `(setf (symbol-function (intern ,func-name))
	   (lambda (arg)
	     (declare (optimize (speed 3) (safety 0)))
	     (let ((parser ,@(construct-parser-call spec 'arg))
		   (context ,@(construct-context-call spec)))
	       (declare (dynamic-extent parser))
	       (declare (type ,parser-type parser))
	       (declare (type ,context-type context))
	       (labels ((next-event ()
			  (let* ((start (the fixnum ,@(single-call spec :pos 'parser)))
				 (current (the character ,@(single-call spec :current-char 'parser))))
			    (loop do (case current
				       (#\Return
					(let ((end (the fixnum ,@(single-call spec :pos 'parser))))
					  (if (char= #\Linefeed ,@(single-call spec :next-char 'parser))
					      ,@(single-call spec :advance 'parser))
					  (return (values :line start end))))
				       
				       (#\Linefeed
					(let ((end (the fixnum ,@(single-call spec :pos 'parser))))
					  ,@(single-call spec :advance 'parser)
					  (return (values :line start end))))
				       
				       (#\Nul
					(let ((end (the fixnum ,@(single-call spec :pos 'parser))))
					  (if (= start end)
					      (return (values :eof -1 -1))
					      (return (values :line start end)))))
				       
				       (otherwise (setf current ,@(single-call spec :next-char 'parser))))))))
		 
		 (loop do (multiple-value-bind (evt start end) (next-event)
			    (declare (type symbol evt))
			    (declare (type fixnum start end))
			    
			    (case evt
			      (:line
			       (let ((read-buffer ,@(single-call spec :read-buffer 'parser)))
				 (setf context ,@(buffer-call spec :on-line 'context 'read-buffer 'start 'end))
			       ,@(single-call spec :reset-buffer 'parser)))
			      (:eof
			       (return ,@(single-call spec :on-eof 'context))))))))))))

#|(defmacro ld
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
  `(ld ,name
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
|#


#|
(ld-2 csv-parse (:parser-type str-parser :context-type cons)
	(:construct-parser (str) (str-parser-construct str))
	(:construct-context () (accum-list-init))
	(:pos (parser) (str-parser-pos parser))
	(:advance (parser) (str-parser-advance parser))
	(:read-buffer (parser) (str-parser-read-buffer parser))
	(:reset-buffer (parser) (call->nil))
	(:next-char (parser) (str-parser-next parser))
	(:current-char (parser) (str-parser-char parser))
	(:on-line (ctx buf start end) (accum-list-strings ctx buf start end))
(:on-eof (ctx) (accum-list-extract ctx)))
|#
