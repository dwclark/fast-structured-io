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

(defparameter *only-props* (asdf:system-relative-pathname "fast-structured-io" "data/ini-only-props.ini"))
(defparameter *w3* (asdf:system-relative-pathname "fast-structured-io" "data/w3schools_sample.ini"))

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
