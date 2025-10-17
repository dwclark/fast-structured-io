(defpackage :fast-structured-csv-tests
  (:nicknames :csv-tests)
  (:use #:cl #:alexandria #:fiveam #:fsio-utils #:fsio-csv))

(in-package :fast-structured-csv-tests)

(defparameter *people-100* (asdf:system-relative-pathname "fast-structured-io" "data/people-100.csv"))
(defparameter *people-10000* (asdf:system-relative-pathname "fast-structured-io" "data/people-10000.csv"))
(defparameter *tsv* (asdf:system-relative-pathname "fast-structured-io" "data/tsv_backescape.csv"))

(def-suite csv-tests)
(in-suite csv-tests)

(test use-table-with-headers-and-transformers
  (let ((tab (table-new :has-headers nil :keep-rows t
			:headers (vector "name" "age" "rating")
			:transformers (vector #'identity #'parse-integer #'read-from-string))))

    (dolist (lst '(("scooby" "10" "12e2") ("shaggy" "30" "13.34e2")))
      (dolist (s lst)
	(table-add-field tab s))
      (table-finalize-current tab))
    
    (is (string= "scooby" (table-get-field tab "name" 0)))
    (is (= 10 (table-get-field tab "age" 0)))
    (is (= 12e2 (table-get-field tab "rating" 0)))

    (is (string= "shaggy" (table-get-field tab 0 1)))f
    (is (= 30 (table-get-field tab 1 1)))
    (is (= 13.34e2 (table-get-field tab 2 1)))))

(test use-table-with-embedded-headers
  (let ((tab (table-new :has-headers t :keep-rows t)))
    (dolist (lst '(("name" "age" "rating") ("scooby" "10" "12e2") ("shaggy" "30" "13.34e2")))
      (dolist (s lst)
	(table-add-field tab s))
      (table-finalize-current tab))

    (is (equalp (vector "name" "age" "rating") (table-headers tab)))
    
    (is (string= "scooby" (table-get-field tab "name" 0)))
    (is (string= "10" (table-get-field tab "age" 0)))
    (is (string= "12e2" (table-get-field tab "rating" 0)))

    (is (string= "shaggy" (table-get-field tab 0 1)))
    (is (string= "30" (table-get-field tab 1 1)))
    (is (string= "13.34e2" (table-get-field tab 2 1)))))

(mixin str-csv->table str-parser str-functions table table-accum)
(mixin str-csv-noops str-parser str-functions list noops)
(mixin stm-tsv->matrix stm-parser stm-functions (vector t *) matrix-accum :sep #\Tab :quot #\' :esc #\\)

(test use-table-people-100
  (let* ((str (uiop:read-file-string *people-100*))
	 (parser (make-str-parser :read-buffer str))
	 (tab (table-new :has-headers t :keep-rows t))
	 (tab (str-csv->table parser tab)))
    (is (equalp (vector "Index" "User Id" "First Name" "Last Name" "Sex" "Email" "Phone" "Date of birth" "Job Title")
		(table-headers tab)))
    (is (= 100 (table-row-count tab)))

    (loop for should-be across (vector "50" "E47FB71DD9ACCd9" "Joshua" "Sweeney" "Male" "daisymcgee@example.net"
				       "875.994.2100x535" "1954-07-28" "Education officer, museum")
	  for col from 0 below 9
	  do (is (string= should-be (table-get-field tab col 49))))

    (is (equalp (vector "100" "b8D0aD3490FC7e1" "Mariah" "Bernard" "Male" "pcopeland@example.org" "(341)594-6554x44657" "2016-11-15" "IT sales professional")
		(table-get-row tab 99)))))

(test parse-tsv
  (with-open-file (fstream *tsv*)
    (let* ((matrix (make-array 1 :adjustable t :fill-pointer t
				 :initial-contents (list (make-array 0 :adjustable t :fill-pointer t))))
	   (tsv (stm-tsv->matrix (stm-parser-new fstream) matrix)))
      (is (= 2 (length matrix)))
      (is (string= "help'" (aref (aref matrix 1) 0)))
      (is (string= "she said, 'hello'" (aref (aref matrix 1) 1))))))

(defun parse-speed ()
  (let* ((str (uiop:read-file-string *people-10000*))
	 (parser (make-str-parser :read-buffer str))
	 (len (length str))
	 (times 1000))
    (format t "str length: ~A~%" (length str))
    (dotimes (var times)
      (declare (ignore var))
      (str-csv-noops parser nil)
      (setf (fsio-utils::str-parser-pos parser) 0))
    
    (format t "finished. total mb: ~A, total rows: ~A~%" (float (/ (* times len) (* 1024 1024))) (* times 10000))))

