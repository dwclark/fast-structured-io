(in-package :fast-structured-io)

(defmacro ld (name (in &key (parser-type t) (context-type t)) &body spec)
  (with-unique-names (parser context start current end evt read-buffer next-event)
    (let ((func-name (symbol-name name)))
      `(setf (symbol-function (intern ,func-name))
	     (lambda (,in)
	       (declare (optimize (speed 3) (safety 0)))
	       (let ((,parser ,@(construct-parser-call spec in))
		     (,context ,@(construct-context-call spec)))
		 (declare (ignorable ,context))
		 (declare (dynamic-extent ,parser))
		 (declare (type ,parser-type ,parser))
		 (declare (type ,context-type ,context))
		 (labels ((,next-event ()
			    (let* ((,start (the fixnum ,@(single-call spec :pos parser)))
				   (,current (the character ,@(single-call spec :current-char parser))))
			      (loop do (case ,current
					 (#\Return
					  (let ((,end (the fixnum ,@(single-call spec :pos parser))))
					    (if (char= #\Linefeed ,@(single-call spec :next-char parser))
						,@(single-call spec :advance parser))
					    (return (values :line ,start ,end))))

					 ;; See https://docs.python.org/3/library/stdtypes.html#str.splitlines
					 ;; for behavior of python splitlines, which this emulates
					 ((#\Linefeed #\Vt #\Page #\Fs #\Gs #\Rs #\Next-Line #\LINE_SEPARATOR #\PARAGRAPH_SEPARATOR)
					  (let ((,end (the fixnum ,@(single-call spec :pos parser))))
					    ,@(single-call spec :advance parser)
					    (return (values :line ,start ,end))))
					 
					 (#\Nul
					  (let ((,end (the fixnum ,@(single-call spec :pos parser))))
					    (if (= ,start ,end)
						(return (values :eof -1 -1))
						(return (values :line ,start ,end)))))
					 
					 (otherwise (setf ,current ,@(single-call spec :next-char parser))))))))
		   
		   (loop do (multiple-value-bind (,evt ,start ,end) (,next-event)
			      (declare (type symbol ,evt))
			      (declare (type fixnum ,start ,end))
			      (declare (ignorable ,start ,end))
			      
			      (case ,evt
				(:line
				 (let ((,read-buffer ,@(single-call spec :read-buffer parser)))
				   (declare (ignorable ,read-buffer))
				   (setf ,context ,@(buffer-call spec :on-line context read-buffer start end))
				   ,@(single-call spec :reset-buffer parser)))
				(:eof
				 (return ,@(single-call spec :on-eof context)))))))))))))

(defmacro ld-str (name (in &key (context-type t)) &body spec)
  `(ld ,name (,in :parser-type str-parser :context-type ,context-type)
     (:construct-parser (str) (make-str-parser :read-buffer str :pos 0))
     (:pos (parser) (str-parser-pos parser))
     (:advance (parser) (str-parser-advance parser))
     (:read-buffer (parser) (str-parser-read-buffer parser))
     (:reset-buffer (parser) nil)
     (:next-char (parser) (str-parser-next parser))
     (:current-char (parser) (str-parser-char parser))
     ,@spec))

(ld-str ld-str-noop (int :context-type list)
  (:construct-context () nil)
  (:on-line (ctx buf start end) nil)
  (:on-eof (ctx) nil))

(ld-str ld-str-count-lines (in :context-type fixnum)
  (:construct-context () 0)
  (:on-line (ctx buf start end) (incf ctx))
  (:on-eof (ctx) ctx))

(ld-str ld-str->list (in :context-type cons)
  (:construct-context () (accum-list-init))
  (:on-line (lst buf start end) (accum-list lst (subseq buf start end)))
  (:on-eof (lst) (accum-list-extract lst)))

(ld-str ld-str->list-ints (in :context-type cons)
  (:construct-context () (accum-list-init))
  (:on-line (lst buf start end) (accum-list lst (parse-integer buf :start start :end end)))
  (:on-eof (lst) (accum-list-extract lst)))

(ld-str ld-str->vec-ints (in :context-type (vector fixnum *))
  (:construct-context () (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (:on-line (vec buf start end) (progn
				  (vector-push-extend (parse-integer buf :start start :end end) vec)
				  vec))
  (:on-eof (vec) vec))
