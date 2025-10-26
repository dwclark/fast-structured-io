(defpackage :fast-structured-io-utils
  (:nicknames :fsio-utils)
  (:use #:cl #:alexandria)
  (:export #:accum-list-init #:accum-list #:accum-list-extract #:replace-symbol
	   #:replace-symbols #:mixin-or-default #:mixin-type #:mixin-call
	   #:str-parser #:make-str-parser #:str-functions
	   #:stm-parser #:stm-parser-new #:stm-functions #:nullable-hash-table
	   #:remove-escapes #:unicode-escape->char #:unicode-escape-p))
  
(defpackage :fast-structured-io-ld
  (:nicknames :fsio-ld)
  (:use #:cl #:alexandria #:fast-structured-io-utils)
  (:export #:ld #:mixin #:noops #:count-lines #:accum-list-strs #:accum-list-ints #:accum-vec-ints))

(defpackage :fast-structured-io-csv
  (:nicknames :fsio-csv)
  (:use #:cl #:alexandria #:fast-structured-io-utils)
  (:export #:csv #:mixin #:chars #:noops #:matrix-accum
	   #:table #:table-new #:table-has-headers #:table-row-count #:table-headers
	   #:table-transformers #:table-header-map #:table-rows #:table-current
	   #:table-new #:table-add-field #:table-get-field #:table-finalize-current #:table-accum
	   #:table-get-row))

(defpackage :fast-structured-io-ini
  (:nicknames :fsio-ini)
  (:use #:cl #:alexandria #:fast-structured-io-utils)
  (:export #:alists #:make-alists #:alists-add-key #:alists-add-value #:alists-finish-group
	   #:alists-add-group-name #:alists-finish #:hashtable-init #:hashtable-accum
	   #:ini #:mixin #:alists-accum))

(defpackage :fast-structured-io-properties
  (:nicknames :fsio-properties)
  (:use #:cl #:alexandria #:fast-structured-io-utils)
  (:export #:remove-properties-escapes #:properties #:mixin #:hash-table-accum #:noops))
