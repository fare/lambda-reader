#-asdf3 (error "asdf3 or bust!")

(in-package :asdf)

(defclass cl-uncompiled-source-file (cl-source-file)
  ())

(defmethod output-files ((o compile-op) (c cl-uncompiled-source-file))
  nil)

(defmethod input-files ((o compile-op) (c cl-uncompiled-source-file))
  nil)

(defmethod input-files ((o load-op) (c cl-uncompiled-source-file))
  (input-files 'load-source-op c))

(defmethod perform ((o compile-op) (c cl-uncompiled-source-file))
  (values))

(defmethod perform ((o load-op) (c cl-uncompiled-source-file))
  (perform 'load-source-op c))

(defsystem "lambda-reader-8bit"
  :version "1.0.0"
  :description "Use unicode character λ for LAMBDA in reader and printer"
  :long-description "Use unicode character λ for LAMBDA in reader and printer;
relying on named-readtables as a sensible readtable API."
  :author "Francois-Rene Rideau"
  :license "MIT"
  :defsystem-depends-on ("asdf-encodings")
  :encoding :latin1 ;; CHEAT!
  :depends-on ("named-readtables")
  :components ((cl-uncompiled-source-file "lambda-reader")))
