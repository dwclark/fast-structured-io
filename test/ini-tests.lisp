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
