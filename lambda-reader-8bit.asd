(in-package :asdf)

(defclass cl-uncompiled-source-file (cl-source-file)
  ())

(defmethod output-files ((operation compile-op) (c cl-uncompiled-source-file))
  (declare (ignorable operation c))
  nil)

(defmethod input-files ((operation compile-op) (c cl-uncompiled-source-file))
  (declare (ignorable operation c))
  nil)

(defmethod input-files ((operation load-op) (c cl-uncompiled-source-file))
  (declare (ignorable operation))
  (input-files (make-instance 'load-source-op) c))

(defmethod perform ((op compile-op) (c cl-uncompiled-source-file))
  (declare (ignorable op c))
  (values))

(defmethod perform ((op load-op) (c cl-uncompiled-source-file))
  (declare (ignorable op))
  (perform (make-instance 'asdf:load-source-op) c))

(defsystem :lambda-reader-8bit
  :defsystem-depends-on (:asdf-encodings)
  #+asdf-unicode :encoding #+asdf-unicode :latin1 ;; CHEAT!
  :description "Use unicode character λ for LAMBDA in reader and printer"
  :long-description "Use unicode character λ for LAMBDA in reader and printer;
relying on named-readtables as a sensible readtable API."
  :depends-on (:named-readtables)
  :components ((cl-uncompiled-source-file "lambda-reader")))
