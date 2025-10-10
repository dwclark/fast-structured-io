(defpackage :fast-structured-io-tests
  (:nicknames :fsio-tests)
  (:use #:cl #:alexandria #:fast-structured-io #:fiveam))

(in-package :fast-structured-io-tests)

(def-suite fsio-tests)
(in-suite fsio-tests)

(test test-read-numbers-str
  (let ((str (uiop:read-file-string "data/nums.txt"))
	(nums (loop for n from 0 below 1024 collecting n)))
    (is (equal (ld-str->list-ints str) nums))
    (is (equal (ld-str->list str) (mapcar #'write-to-string nums)))
    (is (equalp (ld-str->vec-ints str) (make-array 1024 :initial-contents nums)))
    (is (= 1024 (ld-str-count-lines str)))))

(test test-read-numbers-stm
  (with-open-file (fstream "data/nums.txt")
    (is (= 1024 (ld-stm-count-lines fstream)))))
