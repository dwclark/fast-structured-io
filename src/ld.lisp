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

(defmacro ld (name in (&key (parser-type t) (context-type t)) &body spec)
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
					 
					 (#\Linefeed
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

(defmacro ld-str (name in (&key (context-type t)) &body spec)
  `(ld ,name ,in (:parser-type str-parser :context-type ,context-type)
     (:construct-parser (str) (str-parser-construct str))
     (:pos (parser) (str-parser-pos parser))
     (:advance (parser) (str-parser-advance parser))
     (:read-buffer (parser) (str-parser-read-buffer parser))
     (:reset-buffer (parser) 'nil)
     (:next-char (parser) (str-parser-next parser))
     (:current-char (parser) (str-parser-char parser))
     ,@spec))

(ld-str ld-str-noop in (:context-type list)
  (:construct-context () 'nil)
  (:on-line (ctx buf start end) 'nil)
  (:on-eof (ctx) 'nil))

(ld-str ld-str-count-lines in (:context-type fixnum)
  (:construct-context () '0)
  (:on-line (ctx buf start end) (incf ctx))
  (:on-eof (ctx) ctx))

(ld-str ld-str->list in (:context-type cons)
  (:construct-context () (accum-list-init))
  (:on-line (lst buf start end) (accum-list lst (subseq buf start end)))
  (:on-eof (lst) (accum-list-extract lst)))

(ld-str ld-str->list-ints in (:context-type cons)
  (:construct-context () (accum-list-init))
  (:on-line (lst buf start end) (accum-list lst (parse-integer buf :start start :end end)))
  (:on-eof (lst) (accum-list-extract lst)))

(ld-str ld-str->vec-ints in (:context-type (vector fixnum *))
  (:construct-context () (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (:on-line (vec buf start end) (progn
				  (vector-push-extend (parse-integer buf :start start :end end) vec)
				  vec))
  (:on-eof (vec) vec))
