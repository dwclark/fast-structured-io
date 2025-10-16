(in-package :fast-structured-io-tests)

(defparameter *people-100* (asdf:system-relative-pathname "fast-structured-io" "data/people-100.csv"))
;(defparameter *empty.txt* (asdf:system-relative-pathname "fast-structured-io" "data/empty.txt"))
;(defparameter *singleline.txt* (asdf:system-relative-pathname "fast-structured-io" "data/singleline.txt"))

;(def-suite fsio-tests)
(in-suite fsio-tests)

(test use-table-with-headers-and-transformers
  (let ((tab (fsio-csv:table-new :has-headers nil :keep-rows t
				 :headers (vector "name" "age" "rating")
				 :transformers (vector #'identity #'parse-integer #'read-from-string))))

    (dolist (lst '(("scooby" "10" "12e2") ("shaggy" "30" "13.34e2")))
      (dolist (s lst)
	(fsio-csv:table-add-field tab s))
      (fsio-csv:table-finalize-current tab))

    (is (string= "scooby" (fsio-csv:table-get-field tab "name" 0)))
    (is (= 10 (fsio-csv:table-get-field tab "age" 0)))
    (is (= 12e2 (fsio-csv:table-get-field tab "rating" 0)))

    (is (string= "shaggy" (fsio-csv:table-get-field tab 0 1)))
    (is (= 30 (fsio-csv:table-get-field tab 1 1)))
    (is (= 13.34e2 (fsio-csv:table-get-field tab 2 1)))))

(test use-table-with-embedded-headers
  (let ((tab (fsio-csv:table-new :has-headers t :keep-rows t)))
    (dolist (lst '(("name" "age" "rating") ("scooby" "10" "12e2") ("shaggy" "30" "13.34e2")))
      (dolist (s lst)
	(fsio-csv:table-add-field tab s))
      (fsio-csv:table-finalize-current tab))

    (is (equalp (vector "name" "age" "rating") (fsio-csv:table-headers tab)))
    
    (is (string= "scooby" (fsio-csv:table-get-field tab "name" 0)))
    (is (string= "10" (fsio-csv:table-get-field tab "age" 0)))
    (is (string= "12e2" (fsio-csv:table-get-field tab "rating" 0)))

    (is (string= "shaggy" (fsio-csv:table-get-field tab 0 1)))
    (is (string= "30" (fsio-csv:table-get-field tab 1 1)))
    (is (string= "13.34e2" (fsio-csv:table-get-field tab 2 1)))))

(test use-table-people-100
  (let* ((str (uiop:read-file-string *people-100*))
	 (tab (str-csv->table str)))
    (format t "~A~%" (fsio-csv:table-row-count tab))))
