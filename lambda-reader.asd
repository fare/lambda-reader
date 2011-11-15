(defsystem :lambda-reader
  :depends-on (:named-readtables)
  :description "Use unicode character λ for LAMBDA in reader and printer"
  :components ((:file "lambda-reader")))
