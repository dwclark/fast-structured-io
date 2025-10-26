(defpackage :fast-structured-properties-tests
  (:nicknames :properties-tests)
  (:use #:cl #:alexandria #:fiveam #:fsio-utils #:fsio-properties))

(in-package :fast-structured-properties-tests)

(def-suite properties-tests)
(in-suite properties-tests)

(test test-remove-escapes
  (let* ((raw "one\\.two\\.three")
	 (len (length raw)))
    (is (string= "one.two.three" (remove-properties-escapes raw 0 len))))
  
  (let* ((raw "Th\is \is n\ot \esc\ap\ed")
	 (len (length raw)))
    (is (string= "This is not escaped" (remove-properties-escapes raw 0 len))))
  
  (let* ((raw (concatenate 'string "multiline.\\" (list #\Newline) "key"))
	 (len (length raw)))
    (is (string= "multiline.key" (remove-properties-escapes raw 0 len))))
  
  (let* ((raw (concatenate 'string "some \\" (list #\Newline #\Tab #\Tab)
			   "other stuff \\" (list #\Newline) (make-string 10 :element-type 'character :initial-element #\Space)
			   "like this"))
	 (len (length raw)))
    (is (string= "some other stuff like this" (remove-properties-escapes raw 0 len))))
  )

(defparameter *simple.properties* (asdf:system-relative-pathname "fast-structured-io" "data/properties/simple.properties"))
(defparameter *one.properties* (asdf:system-relative-pathname "fast-structured-io" "data/properties/one.properties"))
(defparameter *speed.properties* (asdf:system-relative-pathname "fast-structured-io" "data/properties/speed.properties"))

(mixin str->hash-table str-parser str-functions hash-table hash-table-accum)
(mixin stm->hash-table stm-parser stm-functions hash-table hash-table-accum)

(test simple-properties-test
  (let* ((table-strs
	   (let ((table (make-hash-table :test #'equal))
		 (parser (make-str-parser :read-buffer (uiop:read-file-string *simple.properties*))))
	     (str->hash-table parser table)))
	 (table-stm
	   (let ((table (make-hash-table :test #'equal)))
	     (with-open-file (fstream *simple.properties*)
	       (stm->hash-table (stm-parser-new fstream) table)))))
	     
    (loop for table in (list table-strs table-stm)
	  do (is (string= "foo" (gethash "prop1" table)))
	     (is (string= "bar    " (gethash "prop2" table)))
	     (is (string= "baz" (gethash "prop3" table))))))

(test one-properties-test
  (let* ((table-strs
	   (let ((table (make-hash-table :test #'equal))
		 (parser (make-str-parser :read-buffer (uiop:read-file-string *one.properties*))))
	     (str->hash-table parser table)))
	 (table-stm
	   (let ((table (make-hash-table :test #'equal)))
	     (with-open-file (fstream *one.properties*)
	       (stm->hash-table (stm-parser-new fstream) table)))))

    (loop for table in (list table-strs table-stm)
	  do (is (string= "another blah" (gethash "fooagain" table)))
	     (is (string= "blah" (gethash "foo" table)))
	     (is (string= "followed by a multiline value" (gethash "multiline.key" table)))
	     (is (string= "quoted\" and \"this too\" and this is not quoted" (gethash "this\"is" table)))
	     (is (string= "quoted key\" 'another quoted key'" (gethash "\"my" table)))
	     (is (string= "what the heck is this?" (gethash "ridiculous" table)))
	     (is (string= "This is not escaped" (gethash "skipped.escapes" table)))
	     (is (string= "lucky #7" (gethash "0123" table)))
	     (is (string= "some other stuff like this" (gethash "spring.boot.property" table)))
	     (is (string= "and here is a dumb value" (gethash "stupid.keyis.here" table)))
	     (is (string= "blah" (gethash "foo1" table)))
	     (is (string= "blah" (gethash "foo2" table)))
	     (is (string= "\"my comment\"" (gethash "foo bar baz" table))))))

(mixin str->nil str-parser str-functions list noops)

;; initial tests show ~ 324 MB/s
(defun test-properties-speed ()
  (let ((times 10000)
	(size 200090)
	(parser (make-str-parser :read-buffer (uiop:read-file-string *speed.properties*))))
    (dotimes (i times)
      (declare (ignore i))
      (str->nil parser nil)
      (setf (fsio-utils::str-parser-pos parser) 0))
    (format t "Size parsed ~A MB ~%" (/ (* times size) (* 1024.0 1024.0)))))
  
