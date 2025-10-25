(defpackage :fast-structured-properties-tests
  (:nicknames :properties-tests)
  (:use #:cl #:alexandria #:fiveam #:fsio-utils #:fsio-properties))

(in-package :fast-structured-properties-tests)

(def-suite properties-tests)
(in-suite properties-tests)

(test test-remove-escapes
  (let* ((raw "one\\.two\\.three")
	 (len (length raw)))
    (dolist (item '(t nil))
      (is (string= "one.two.three" (remove-properties-escapes raw 0 len item)))))
  
  (let* ((raw "Th\is \is n\ot \esc\ap\ed")
	 (len (length raw)))
    (dolist (item '(t nil))
      (is (string= "This is not escaped" (remove-properties-escapes raw 0 len item)))))

  (let* ((raw (concatenate 'string "multiline.\\" (list #\Newline) "key"))
	 (len (length raw)))
    (is (string= "multiline.key" (remove-properties-escapes raw 0 len nil))))

  (let* ((raw (concatenate 'string "some \\" (list #\Newline #\Tab #\Tab)
			   "other stuff \\" (list #\Newline) (make-string 10 :element-type 'character :initial-element #\Space)
			   "like this"))
	 (len (length raw)))
    (is (string= "some other stuff like this" (remove-properties-escapes raw 0 len t))))
  )
