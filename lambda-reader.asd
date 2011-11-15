(defsystem :lambda-reader
  :description "Use unicode character λ for LAMBDA in reader and printer"
  :long-description "Use unicode character λ for LAMBDA in reader and printer;
relying on named-readtables as a sensible readtable API."
  :depends-on (:named-readtables)
  :components ((:file "lambda-reader")))
