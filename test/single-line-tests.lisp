(defpackage :fast-structured-ld-tests
  (:nicknames :ld-tests)
  (:use #:cl #:alexandria #:fiveam #:fsio-utils #:fsio-ld))

(in-package :fast-structured-ld-tests)

(defparameter *nums.txt* (asdf:system-relative-pathname "fast-structured-io" "data/nums.txt"))
(defparameter *empty.txt* (asdf:system-relative-pathname "fast-structured-io" "data/empty.txt"))
(defparameter *singleline.txt* (asdf:system-relative-pathname "fast-structured-io" "data/singleline.txt"))

(def-suite ld-tests)
(in-suite ld-tests)

(mixin ld-str-noop str-parser str-functions list noops)
(mixin ld-str-count-lines str-parser str-functions fixnum count-lines)
(mixin ld-str->list str-parser str-functions cons accum-list-strs)
(mixin ld-str->list-ints str-parser str-functions cons accum-list-ints)
(mixin ld-str->vec-ints str-parser str-functions (vector fixnum *) accum-vec-ints)

(mixin ld-stm-noop stm-parser stm-functions list noops)
(mixin ld-stm-count-lines stm-parser stm-functions fixnum count-lines)
(mixin ld-stm->list stm-parser stm-functions cons accum-list-strs)
(mixin ld-stm->list-ints stm-parser stm-functions cons accum-list-ints)
(mixin ld-stm->vec-ints stm-parser stm-functions (vector fixnum *) accum-vec-ints)

(test test-read-numbers-str
  (let ((str (uiop:read-file-string *nums.txt*))
	(nums (loop for n from 0 below 1024 collecting n)))
    (is (= 1024 (ld-str-count-lines (make-str-parser :read-buffer str :pos 0) 0)))
    (is (= 0 (ld-str-count-lines (make-str-parser :read-buffer (uiop:read-file-string *empty.txt*) :pos 0) 0)))
    (is (= 1 (ld-str-count-lines (make-str-parser :read-buffer (uiop:read-file-string *singleline.txt*) :pos 0) 0)))
    (is (equal nums (ld-str->list-ints (make-str-parser :read-buffer str :pos 0) (accum-list-init))))
    (is (equal (mapcar #'write-to-string nums)
	       (ld-str->list (make-str-parser :read-buffer str :pos 0) (accum-list-init))))
    (is (equalp (make-array 1024 :initial-contents nums)
		(ld-str->vec-ints (make-str-parser :read-buffer str :pos 0)
				  (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t))))))

(test test-read-numbers-stm
  (let ((nums (loop for n from 0 below 1024 collecting n)))
    (with-open-file (fstream *nums.txt*)
      (is (= 1024 (ld-stm-count-lines (stm-parser-new fstream) 0))))
    
    (with-open-file (fstream *empty.txt*)
      (is (= 0 (ld-stm-count-lines (stm-parser-new fstream) 0))))

    (with-open-file (fstream *singleline.txt*)
      (is (= 1 (ld-stm-count-lines (stm-parser-new fstream) 0))))
    
    (with-open-file (fstream *nums.txt*)
      (is (equal nums (ld-stm->list-ints (stm-parser-new fstream) (accum-list-init)))))

    (with-open-file (fstream *nums.txt*)
      (is (equal (mapcar #'write-to-string nums)
		 (ld-stm->list (stm-parser-new fstream) (accum-list-init)))))

    (with-open-file (fstream *nums.txt*)
      (is (equalp (make-array 1024 :initial-contents nums)
		  (ld-stm->vec-ints (stm-parser-new fstream)
				    (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)))))))
