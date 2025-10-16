(in-package :fast-structured-io-ld)

(defun noops()
  '((:on-line (ctx buf start end) nil)
    (:on-eof (ctx) nil)))

(defun count-lines ()
  '((:on-line (ctx buf start end) (incf ctx))
    (:on-eof (ctx) ctx)))

(defun accum-list-strs ()
  '((:on-line (lst buf start end) (accum-list lst (subseq buf start end)))
    (:on-eof (lst) (accum-list-extract lst))))

(defun accum-list-ints ()
  '((:on-line (lst buf start end) (accum-list lst (parse-integer buf :start start :end end)))
    (:on-eof (lst) (accum-list-extract lst))))

(defun accum-vec-ints ()
  '((:on-line (vec buf start end)
     (progn
       (vector-push-extend (parse-integer buf :start start :end end) vec)
       vec))
    (:on-eof (vec) vec)))

