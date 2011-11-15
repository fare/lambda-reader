;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Reader for λ and/or Λ that returns cl:lambda.
;;; Legal mumbo-jumbo at bottom of file

#|;;; Use it as follows:
 (asdf:load-system :lambda-reader)
 (named-readtables:in-readtable :λ-standard)
 (defpackage :FOO (:use :cl) (:import-from :λ-reader #:λ))
 (in-package :FOO)
 (funcall ((λ(x) (funcall x x)) (λ (f) (λ (x) (if (<= x 2) 1 (* x (funcall (funcall f f) (1- x))))))) 6)
|#
;;; When using the :λ-standard readtable, the :λ-modern readtable,
;;; or any readtable created using define-λ-readtable,
;;; any token starting with λ or Λ that would be read as the symbol λ-reader:λ
;;; is now read as cl:lambda instead.
;;; Moreover, it will be pretty-printed as λ-reader:λ would; thus,
;;; if the printer case is uppercase, Λ will be printed; otherwise λ will be.
;;; You can define your own readtables using λ with define-λ-readtable,
;;; which is a variant of named-readtables:defreadtable that does magic for λ.

#+xcvb (module ())

(defpackage :λ-reader (:use :cl :named-readtables)
  (:nicknames #:lambda-readtable)
  (:export #:λ #:make-λ-reader #:install-λ-printer
           #:install-λ-reader #:new-readtable-with-λ-reader #:define-λ-readtable))

(in-package :λ-reader)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *lambda-symbol*
    (let ((*package* (find-package :λ-reader))
          (*readtable* (find-readtable :standard)))
      (read-from-string "λ"))
    "Magic lambda symbol"))

(defmacro #.*lambda-symbol* (&body args)
  (declare (ignore args))
  (error "Use ~S, ~S or ~S before using the λ reader macro."
         'install-λ-reader 'new-readtable-with-λ-reader 'define-λ-readtable))

(define-symbol-macro #.*lambda-symbol* (#.*lambda-symbol*))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *readtable-λ-reader* (make-hash-table :test 'equal))
(defparameter *λ-reader-readtable* (make-hash-table :test 'eql))

(defun make-λ-reader (&optional (readtable *readtable*))
  (setf readtable (find-readtable readtable))
  (or (gethash readtable *readtable-λ-reader*)
      (let ((reader
             (lambda (stream char)
               (let* ((lstream (make-concatenated-stream
                                (make-string-input-stream (string char))
                                stream))
                      (*readtable* readtable)
                      (read-symbol (read lstream t nil t)))
                 (if (eq read-symbol *lambda-symbol*)
                     'lambda
                     read-symbol)))))
        (setf (gethash reader *λ-reader-readtable*) readtable)
        (setf (gethash readtable *readtable-λ-reader*) reader)
        reader)))

(defun λ-reader-p (x)
  (nth-value 1 (gethash x *λ-reader-readtable*)))

(defun λ-printer (stream object)
  (if (and
       (λ-reader-p (get-macro-character
                    (ecase *print-case*
                      ((:upcase :capitalize) #\Λ)
                      (:downcase #\λ))))
       (eq (find-symbol (symbol-name *lambda-symbol*) *package*) *lambda-symbol*))
      (write *lambda-symbol* :stream stream)
      (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
        (set-pprint-dispatch '(eql lambda) nil)
        (write object :stream stream))))

(defun install-λ-printer ()
  (set-pprint-dispatch '(eql lambda) #'λ-printer))

(defun install-λ-reader-helper (old-readtable new-readtable)
  (set-macro-character #\λ (make-λ-reader old-readtable) t new-readtable)
  (set-macro-character #\Λ (make-λ-reader old-readtable) t new-readtable)
  (install-λ-printer)
  nil)

(defun install-λ-reader (&optional (readtable *readtable*))
  (let ((old-readtable (copy-readtable readtable)))
    (install-λ-reader-helper old-readtable readtable)
    readtable))

(defun new-readtable-with-λ-reader (&optional (readtable *readtable*))
  (let ((new-readtable (copy-readtable readtable)))
    (install-λ-reader-helper readtable new-readtable)
    new-readtable))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (install-λ-printer)
  (defreadtable :λ-common-lisp
    (:fuze :common-lisp)
    (:macro-char #\λ (make-λ-reader :common-lisp) t)
    (:macro-char #\Λ (make-λ-reader :common-lisp) t))
  (defreadtable :λ-standard
    (:merge :λ-common-lisp))
  (defreadtable :λ-modern
    (:fuze :modern)
    (:macro-char #\λ (make-λ-reader :modern) t)
    (:macro-char #\Λ (make-λ-reader :modern) t)))

(defmacro define-λ-readtable (name &body options)
  `(progn
     (defreadtable ,name ,@options)
     (install-λ-reader (find-readtable ',name))))

); eval-when

;;; Copyright (c) 2008 Brian Mastenbrook
;;; Copyright (c) 2011 Faré Rideau <fare@tunes.org>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Initially from http://paste.lisp.org/display/72575
