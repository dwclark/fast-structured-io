(in-package :fast-structured-io)

(defun noop-functions()
  '((:context-type list)
    (:construct-context () nil)
    (:on-line (ctx buf start end) nil)
    (:on-eof (ctx) nil)))

(defun count-lines-functions ()
  '((:context-type fixnum)
    (:construct-context () 0)
    (:on-line (ctx buf start end) (incf ctx))
    (:on-eof (ctx) ctx)))

(defun accum-list-functions ()
  '((:context-type cons)
    (:construct-context () (accum-list-init))
    (:on-line (lst buf start end) (accum-list lst (subseq buf start end)))
    (:on-eof (lst) (accum-list-extract lst))))

(defun accum-list-ints-functions ()
  '((:context-type cons)
    (:construct-context () (accum-list-init))
    (:on-line (lst buf start end) (accum-list lst (parse-integer buf :start start :end end)))
    (:on-eof (lst) (accum-list-extract lst))))

(defun accum-vec-ints-functions ()
  '((:context-type (vector fixnum *))
    (:construct-context () (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
    (:on-line (vec buf start end) (progn
				    (vector-push-extend (parse-integer buf :start start :end end) vec)
				    vec))
    (:on-eof (vec) vec)))

