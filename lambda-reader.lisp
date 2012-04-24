;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp; coding: utf-8 -*-
;;; Reader for λ and/or Λ that returns cl:lambda.
;;; ♡ Copying is an act of love. Please copy. — Faré Rideau
;;; Legal mumbo-jumbo at bottom of file.
;;;
;;; WARNING ABOUT ENCODING:
;;; Note that this file uses UTF-8, at least,
;;; the original source file as distributed by the author does.
;;; However, if you use an implementation that does not recognize UTF-8,
;;; and instead has 8-bit characters, it should still work,
;;; and other files should still be able to use its functionality, provided
;;; (1) you either do NOT transcode either this file or the files that use it,
;;;  or you do it in compatible ways so that lambdas are still recognized
;;;  as the same characters or sequences of characters, and
;;; (2) when using an implementation that does not recognize
;;;  this file's encoding (either UTF-8 or transcoded),
;;;  you do not care too much that lambdas be read
;;;  as a single character or any particular sequence of characters, and
;;; (3) you are not unhappy with how your implementation prints a λ,
;;;  though it may at times "lowercase" or "uppercase" it in some way;
;;;  you are responsible for controlling encoding and case
;;;  to achieve the effects you want.
;;; For instance, reading this UTF-8 encoded file in an 8-bit implementation
;;; will read a λ as two characters of code #xCE #xBB
;;; instead of one character of code #x3BB;
;;; on a latin1 implementation, this will be characters Î»,
;;; which is uppercase and may be downcased as î» (code #xEE #xBB).
;;; We make every effort so it will work in this and all other cases.


#|;;; Use it as follows:
 (asdf:load-system :lambda-reader)
 (named-readtables:in-readtable :λ-standard)
 (defpackage :FOO (:use :cl :λ-reader))
 ;;OR a stricter: (defpackage :FOO (:use :cl) (:import-from :λ-reader #:λ))
 (in-package :FOO)
 ;; Factorial 10 using λ's and a Y-combinator.
 (funcall ((λ(x) (funcall x x)) (λ (f) (λ (x) (if (<= x 2) 1 (* x (funcall (funcall f f) (1- x))))))) 10)
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
  ;;(:nicknames #:lambda-reader)
  (:export #:λ #:make-λ-reader #:install-λ-printer
           #:install-λ-reader #:new-readtable-with-λ-reader #:define-λ-readtable))

(in-package :λ-reader)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *lambda-string* "λ")
  (defvar *lowercase-lambda-string* (string-downcase *lambda-string*)) ; λ or ???
  (defvar *uppercase-lambda-string* (string-upcase *lambda-string*)) ; Λ or ???
  (defvar *lowercase-lambda-char* (char *lowercase-lambda-string* 0))
  (defvar *uppercase-lambda-char* (char *uppercase-lambda-string* 0))
  (defvar *lambda-symbol*
    (let ((*package* (find-package :λ-reader))
          (*readtable* (find-readtable :standard)))
      (read-from-string *lambda-string*))
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
                      ((:upcase :capitalize) #.*uppercase-lambda-char*)
                      (:downcase #.*lowercase-lambda-char*))))
       (eq (find-symbol *uppercase-lambda-string* *package*) *lambda-symbol*))
      (write *lambda-symbol* :stream stream)
      (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
        (set-pprint-dispatch '(eql lambda) nil)
        (write object :stream stream))))

(defun install-λ-printer ()
  ;; copy the pprint-dispatch table in case it's the immutable default, such as in CCL
  (setf *print-pprint-dispatch* (copy-pprint-dispatch)) 
  (set-pprint-dispatch '(eql lambda) #'λ-printer))

(defun install-λ-reader-helper (old-readtable new-readtable)
  (set-macro-character *lowercase-lambda-char* (make-λ-reader old-readtable) t new-readtable)
  (unless (eql *lowercase-lambda-char* *uppercase-lambda-char*)
    (set-macro-character *uppercase-lambda-char* (make-λ-reader old-readtable) t new-readtable))
  (install-λ-printer)
  new-readtable)

(defun install-λ-reader (&optional (readtable *readtable*))
  (let* ((new-readtable (find-readtable readtable))
         (old-readtable (copy-readtable new-readtable)))
    (install-λ-reader-helper old-readtable new-readtable)))

(defun new-readtable-with-λ-reader (&optional (readtable *readtable*))
  (let* ((old-readtable (find-readtable readtable))
         (new-readtable (copy-readtable old-readtable)))
    (install-λ-reader-helper old-readtable new-readtable)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (install-λ-printer)
  (defreadtable :λ-common-lisp
    (:fuze :common-lisp)
    (:macro-char #.*lowercase-lambda-char* (make-λ-reader :common-lisp) t)
    (:macro-char #.*uppercase-lambda-char* (make-λ-reader :common-lisp) t))
  (defreadtable :λ-standard
    (:merge :λ-common-lisp))
  #|(defreadtable :lambda-standard (:merge :λ-standard))|#
  (defreadtable :λ-modern
    (:fuze :modern)
    (:macro-char #.*lowercase-lambda-char* (make-λ-reader :modern) t)
    (:macro-char #.*uppercase-lambda-char* (make-λ-reader :modern) t))
  #|(defreadtable :lambda-modern (:merge :λ-modern))|#)

(defmacro define-λ-readtable (name &body options)
  `(progn
     (defreadtable ,name ,@options)
     (install-λ-reader ',name)))

); eval-when

;;; Copyright (c) 2008 Brian Mastenbrook
;;; Copyright (c) 2011-2012 Faré Rideau <fare@tunes.org>

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
