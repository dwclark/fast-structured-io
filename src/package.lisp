(defpackage :fast-structured-io
  (:nicknames :fsio)
  (:use #:cl #:alexandria)
  (:export #:ld
	   #:ld-str-noop #:ld-str-count-lines #:ld-str->list #:ld-str->list-ints #:ld-str->vec-ints
	   #:ld-stm-noop #:ld-stm-count-lines #:ld-stm->list #:ld-stm->list-ints #:ld-stm->vec-ints))
