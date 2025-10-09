(defpackage :fast-structured-io
  (:nicknames :fsio)
  (:use #:cl #:alexandria)
  (:export #:ld #:ld-str #:ld-str-noop #:ld-str-count-lines #:ld-str->list #:ld-str->list-ints #:ld-str->vec-ints))
