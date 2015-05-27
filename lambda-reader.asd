(defsystem "lambda-reader"
  :version "1.0.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :description "Use unicode character λ for LAMBDA in reader and printer"
  :long-description "Use unicode character λ for LAMBDA in reader and printer
relying on named-readtables as a sensible readtable API."
  :author "Francois-Rene Rideau"
  :license "MIT"
  :depends-on ("named-readtables")
  :components ((:file "lambda-reader")))
