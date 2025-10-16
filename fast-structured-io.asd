(asdf:defsystem #:fast-structured-io
  :description "Fast Structured IO"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :components ((:file "src/package")
	       (:file "src/utils")
	       (:file "src/reader")
	       (:file "src/ld-mixins" :depends-on ("src/utils"))
	       (:file "src/ld" :depends-on ("src/ld-mixins"))))
	       ;(:file "src/csv-mixins" :depends-on ("src/utils"))
	       ;(:file "src/csv" :depends-on ("src/csv-mixins"))))

(asdf:defsystem #:fast-structured-io-tests
  :description "Fast Structured IO Tests"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t

  :depends-on ("fast-structured-io" "fiveam" "alexandria" "uiop")

  :components ((:file "test/single-line-tests")))
	       ;(:file "test/csv-tests")))

