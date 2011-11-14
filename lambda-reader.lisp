;;; Reader for λ and Λ that returns CL:LAMBDA.
;;; To use, call Λ-READER:INSTALL-Λ-READER.
;;; Any token starting with λ or Λ that would be read as the symbol Λ-READER:Λ
;;; is now read as CL:LAMBDA.

;;; If the symbol Λ-READER:Λ is imported into the current package,
;;; then CL:LAMBDA will be pretty printed as Λ-READER:Λ would.
;;; Thus, if the printer case is uppercase, Λ will be printed;
;;; otherwise λ will be.

;;; Legal mumbo-jumbo at bottom of file

(defpackage :λ-reader (:use :cl) (:export :install-λ-reader :λ))
(in-package :λ-reader)

(defvar *saved-readtable* (copy-readtable))

(defmacro #.(intern "Λ" *package*) (&body args)
  (declare (ignore args))
  (error "Call ~S before using the λ reader macro."
         'install-λ-reader))

(define-symbol-macro #.(intern "Λ" *package*) (#.(intern "Λ" *package*)))

(defun λ-reader (stream char)
  (let ((stream (make-concatenated-stream
		 (make-string-input-stream (string char))
		 stream))
	(*readtable* *saved-readtable*))
	;; Chandler's original is less efficient but compatible with any user's readtable:
	;;(*readtable* (copy-readtable)))
	;;(set-syntax-from-char char char)
    (let ((read-symbol (read stream t nil t)))
      (cond
        ((eq read-symbol '#.(intern "Λ" *package*))
         'lambda)
        (t read-symbol)))))

(defun λ-printer (stream object)
  (cond
    ((and
      (ecase *print-case*
        ((:upcase :capitalize) (eq (get-macro-character #\Λ) #'λ-reader))
        (:downcase (eq (get-macro-character #\λ) #'λ-reader)))
      (eq (find-symbol "Λ" *package*)
          '#.(intern "Λ" *package*)))
     (write '#.(intern "Λ" *package*) :stream stream))
    (t (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
         (set-pprint-dispatch '(eql lambda) nil)
         (write object :stream stream)))))

(defun install-λ-reader ()
  (set-macro-character #\λ #'λ-reader t)
  (set-macro-character #\Λ #'λ-reader t)
  (set-pprint-dispatch '(eql lambda) #'λ-printer))

;;; Copyright (c) 2008 Brian Mastenbrook

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
