;;; -*- mode:lisp;coding:utf-8 -*-
;;;
;;; Copyright (C) 2011 Wukix, Inc. (http://wukix.com)
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  * Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;
;;;  * Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :wu-decimal
  (:use :cl)
  (:export #:parse-decimal
           #:decimal->string
	   #:enable-reader-macro
	   #:enable-decimal-printing-for-ratios
	   #:disable-decimal-printing-for-ratios))

(in-package :wu-decimal)


(deftype decimal ()
  `(and ratio (satisfies has-finite-decimal-digits)))

(defun has-finite-decimal-digits (ratio)
  (is-power-of-2 (factor-out-powers-of-5 (denominator ratio))))

(defun factor-out-powers-of-2 (number)
  (do ()
      ((or (oddp number) (zerop number)) number)
    (setf number (/ number 2))))

(defun factor-out-powers-of-5 (numbr)
  (do ()
      ((or 
	(plusp (mod numbr 5))
	(zerop numbr))
       numbr)
    (setf numbr (/ numbr 5))))

(defun is-power-of-5 (number)
  ;; Note 1: Not using log, because it uses floats and becomes insufficient for large inputs.
  ;; Note 2: It is intentional that 1 (i.e. 5^0) returns T.
  (do ((x (abs number) (/ x 5)))
      ((<= x 1) (and (= x 1) (/= number -1)))))

(defun is-power-of-2 (number)
  ;; Note: It is intentional that 1 (i.e. 2^0) returns T.
  (let ((absnumber (abs number)))
    (and
     (zerop (boole boole-and absnumber (1- absnumber)))
     (/= number 0 -1))))

(defun enable-decimal-printing-for-ratios ()
  (set-pprint-dispatch 
   'decimal 
   (lambda (str ratio)
     (when *print-escape*
       (princ "#$" str))
     (format str "~a" (decimal->string ratio)))))

(defun disable-decimal-printing-for-ratios ()
  (set-pprint-dispatch 'decimal nil))

(defun enable-reader-macro ()
  (set-dispatch-macro-character #\# #\$ #'decimal-reader))

(defun decimal-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (parse-decimal
   (with-output-to-string (str)
     (loop for c = (read-char stream nil)
	while (and c (or (digit-char-p c) (member c '(#\. #\-))))
	do (write-char c str)
	finally (when c (unread-char c stream))))))

(defun parse-decimal (string) 
  (let* ((period-index (position #\. string))
	 (left (if period-index 
		   (subseq string 0 period-index) 
		   string))
	 (right (if period-index 
		    (subseq string (1+ period-index))
		    nil))
	 (fraction-denominator (make-string (1+ (length right)) :initial-element #\0))
	 (negative (and (> (length left) 0) (char= (elt left 0) #\-)))
	 (parsed-left (if (> (length left) 0) 
			  (if (= (length left) 1)
			      (if (char= (elt left 0) #\-)
				  0
				  (parse-integer left))
			      (parse-integer left))
			  0)))
    (setf (elt fraction-denominator 0) #\1)
    (let ((abs-parsed (abs parsed-left)))
      (when right 
	(setf abs-parsed
	      (+ abs-parsed
		 (read-from-string 
		  (concatenate 'string right "/" fraction-denominator)))))
      (if negative 
	  (- abs-parsed)
	  abs-parsed))))

(defun integer-digit-count (integer)
  ;; Note: Not using log, because it uses floats and becomes insufficient for large inputs.
  (unless (integerp integer) (error "Argument ~a is not an integer." integer))
  (length (format nil "~a" (abs integer))))

(defun integer-digit-count-using-division (integer)
  (unless (integerp integer) (error "Argument ~a is not an integer." integer))
  (setf integer (abs integer))
  (if (zerop integer) 
      1 
      (do ((i 0 (1+ i)))
	  ((< integer 1) i)
	(setf integer (/ integer 10)))))

(defun decimal->string (decimal)
  (when (and (typep decimal 'ratio) 
	     (not (typep decimal 'decimal))) 
    (error "Argument ~a is not a decimal." decimal))
  (let* ((denominator (denominator decimal))
	 (new-denominator 
	  (do ((d (expt 10 (ceiling (log denominator 10)))
		  (* 10 d)))
	      ((zerop (mod d denominator)) 
	       d))))
    (let* ((multiplier (/ new-denominator denominator))
	   (new-numerator (* multiplier (numerator decimal))))
      (multiple-value-bind (whole-part fractional-part) 
	  (truncate (abs new-numerator) new-denominator)
	(let ((zeros (if (zerop fractional-part) 
			 ""
			 (make-string 
			  (ceiling (- (integer-digit-count new-denominator) (integer-digit-count fractional-part) 1))
			  :initial-element #\0))))
	  (with-output-to-string (s)
	    (when (minusp decimal) (princ #\- s))
	    (format s "~a.~a~a" whole-part zeros fractional-part)))))))



