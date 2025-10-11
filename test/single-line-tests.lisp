(defpackage :fast-structured-io-tests
  (:nicknames :fsio-tests)
  (:use #:cl #:alexandria #:fast-structured-io #:fiveam))

(in-package :fast-structured-io-tests)

(def-suite fsio-tests)
(in-suite fsio-tests)

(test test-read-numbers-str
  (let ((str (uiop:read-file-string "data/nums.txt"))
	(nums (loop for n from 0 below 1024 collecting n)))
    (is (= 1024 (ld-str-count-lines str)))
    (is (equal nums (ld-str->list-ints str)))
    (is (equal (mapcar #'write-to-string nums) (ld-str->list str)))
    (is (equalp (make-array 1024 :initial-contents nums) (ld-str->vec-ints str)))))

(test test-read-numbers-stm
  (let ((nums (loop for n from 0 below 1024 collecting n)))
    (with-open-file (fstream "data/nums.txt")
      (is (= 1024 (ld-stm-count-lines fstream))))
    
    (with-open-file (fstream "data/nums.txt")
      (is (equal nums (ld-stm->list-ints fstream))))

    (with-open-file (fstream "data/nums.txt")
      (is (equal (mapcar #'write-to-string nums) (ld-stm->list fstream))))

    (with-open-file (fstream "data/nums.txt")
      (is (equalp (make-array 1024 :initial-contents nums) (ld-stm->vec-ints fstream))))))

