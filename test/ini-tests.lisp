(defpackage :fast-structured-ini-tests
  (:nicknames :ini-tests)
  (:use #:cl #:alexandria #:fiveam #:fsio-utils #:fsio-ini))

(in-package :fast-structured-ini-tests)

(def-suite ini-tests)
(in-suite ini-tests)

(test basic-alists
  (let ((basic (make-alists)))
    (alists-add-group-name basic "section1")
    (loop for (k v) in '(("one" "1") ("two" "2"))
	  do (alists-add-key basic k)
	     (alists-add-value basic v))

    (alists-add-group-name basic "section2")
    (loop for (k v) in '(("three" "3") ("four" "4"))
	  do (alists-add-key basic k)
	     (alists-add-value basic v))

    (is (equalp '(("section1" . (("one" . "1") ("two" . "2"))) ("section2" . (("three" . "3") ("four" . "4"))))
		(alists-finish basic)))))

(test with-global-properties
  (let ((global (make-alists)))
    (loop for (k v) in '(("one" "1") ("two" "2"))
	  do (alists-add-key global k)
	     (alists-add-value global v))
    (alists-add-group-name global "the section")
    (loop for (k v) in '(("one" "i") ("two" "ii"))
	  do (alists-add-key global k)
	     (alists-add-value global v))

    (is (equalp '(("" . (("one" . "1") ("two" . "2"))) ("the section" . (("one" . "i") ("two". "ii"))))
		(alists-finish global)))))

(test java-properties
  (let ((holder (make-alists)))
    (loop for (k v) in '(("one" "1") ("two" "2") ("three" "3") ("four" "4"))
	  do (alists-add-key holder k)
	     (alists-add-value holder v))
    
    (is (equalp '(("one" . "1") ("two" . "2") ("three" . "3") ("four" . "4"))
		(alists-finish holder)))))

(defparameter *only-props* (asdf:system-relative-pathname "fast-structured-io" "data/ini/only-props.ini"))
(defparameter *w3* (asdf:system-relative-pathname "fast-structured-io" "data/ini/w3schools.ini"))
(defparameter *ugly* (asdf:system-relative-pathname "fast-structured-io" "data/ini/ugly.ini"))
(defparameter *python* (asdf:system-relative-pathname "fast-structured-io" "data/ini/python.ini"))

(mixin str-ini->alists str-parser str-functions alists alists-accum)

(test ini-only-props
  (let* ((str (uiop:read-file-string *only-props*))
 	 (parser (make-str-parser :read-buffer str))
  	 (alist (str-ini->alists parser (make-alists))))
    (is (equal '(("one" . "1") ("two" . "2") ("three" . "3")) alist))))

(test w3-schools
  (let* ((str (uiop:read-file-string *w3*))
 	 (parser (make-str-parser :read-buffer str))
 	 (alist (str-ini->alists parser (make-alists)))
	 (should-be '(("http" ("port" . "8080") ("username" . "httpuser"))
		      ("https" ("port" . "8043") ("username" . "httpsuser"))
		      ("FTP" ("port" . "8043") ("username" . "ftpuser"))
		      ("database" ("driverclass" . "com.mysql.jdbc.Driver")
		       ("dbName" . "mydatabase") ("port" . "3306") ("username" . "root")
		       ("password" . "secure"))
		      ("settings" ("enable_ssl" . "true") ("enable_2mf" . "true")))))
    (is (equal should-be alist))))

(test ugly
  (let* ((str (uiop:read-file-string *ugly*))
 	 (parser (make-str-parser :read-buffer str))
 	 (alist (str-ini->alists parser (make-alists)))
	 (should-be '(("ugly" ("why:all=the\\escapes" . "and here is some 
line continuations. why do people 
do this?")
		       ("some \\\"quotes:=" . "some:=\\\" more quotes!!")))))
    (is (equal should-be alist))))
	 
(test python
  (let* ((str (uiop:read-file-string *python*))
 	 (parser (make-str-parser :read-buffer str))
 	 (alist (str-ini->alists parser (make-alists)))
	 (should-be '(("Simple Values" ("key" . "value") ("spaces in keys" . "allowed")
		       ("spaces in values" . "allowed as well")
		       ("spaces around the delimiter" . "obviously")
		       ("you can also use" . "to delimit keys from values"))
		      ("All Values Are Strings" ("values like this" . "1000000") ("or this" . "3.14159265359")
		       ("are they treated as numbers?" . "no")
		       ("integers, floats and booleans are held as" . "strings")
		       ("can use the API to get converted values directly" . "true"))
		      ("Sections Can Be Indented" ("can_values_be_as_well" . "True")
		       ("does_that_mean_anything_special" . "False")
		       ("purpose" . "formatting for readability")))))
    (is (equal should-be alist))))
