;;; -*- mode:lisp;coding:utf-8 -*-
;;;
;;; Copyright (C) 2011-2012 Wukix, Inc. (http://wukix.com)
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

;;; Note: Several printing/formatting-related functions were adapted from
;;; public domain SBCL code.

#+xcvb (module ())

(in-package :cl-user)

(defpackage :wu-decimal
  (:use :cl)
  (:export
   #:*print-decimal-mark*
   #:*print-precision-loss*
   #:decimal
   #:parse-decimal
   #:enable-reader-macro
   #:enable-decimal-printing-for-ratios
   #:disable-decimal-printing-for-ratios
   #:f
   #:$))

(in-package :wu-decimal)

(defparameter *print-decimal-mark* #\.)
(defparameter *print-precision-loss* :error
  "Precision loss mode. Must be :error, :round, or :truncate")

(deftype decimal ()
  `(and rational (satisfies has-finite-decimal-digits)))

(deftype decimal-for-pprint ()
  `(and ratio (satisfies has-finite-decimal-digits)))

(defun has-finite-decimal-digits (number)
  (when (typep number 'rational)
    (is-power-of-2 (factor-out-powers-of-5 (denominator number)))))

(defun factor-out-powers-of-5 (numbr)
  (do ()
      ((or 
	(plusp (mod numbr 5))
	(zerop numbr))
       numbr)
    (setf numbr (/ numbr 5))))

(defun is-power-of-2 (number)
  ;; Note 1: It is intentional that 1 (i.e. 2^0) returns T.
  (assert (plusp number))
  (zerop (logand number (1- number))))

(defun enable-decimal-printing-for-ratios ()
  (set-pprint-dispatch 
   'decimal-for-pprint
   (lambda (str num)
     (when *print-escape*
       (princ "#$" str))
     (format-fixed-aux str num nil nil nil nil nil nil))))

(defun disable-decimal-printing-for-ratios ()
  (set-pprint-dispatch 'decimal-for-pprint nil))

(defun enable-reader-macro (&key exponent-allowed)
  (check-type exponent-allowed boolean)
  (set-dispatch-macro-character #\# #\$ (if exponent-allowed 
					    #'decimal-reader-exponent-allowed
					    #'decimal-reader)))

(defun decimal-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (parse-decimal
   (with-output-to-string (str)
     (loop for c = (read-char stream nil)
	while (and c (or (digit-char-p c) (find c ".-+" :test #'char=)))
	do (write-char c str)
	finally (when c (unread-char c stream))))))

(defun decimal-reader-exponent-allowed (stream subchar arg)
  (declare (ignore subchar arg))
  (parse-decimal
   (with-output-to-string (str)
     (loop for c = (read-char stream nil)
	while (and c (or (digit-char-p c) (find c ".-+eE" :test #'char=)))
	do (write-char c str)
	finally (when c (unread-char c stream))))
   :exponent-allowed t))

(defun whitespace-1-p (a-char)
  (member a-char '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page) :test #'char=))

;;; CLHS does not define any official condition for out of bounds errors,
;;; although this is a common concern as noted in issue SUBSEQ-OUT-OF-BOUNDS.
(define-condition out-of-bounds-error () ()
  (:report "The values of START and/or END are invalid or out of bounds on STRING"))

(define-condition incompatible-mark-and-separator-error () ()
  (:report "DECIMAL-MARK and GROUP-SEPARATOR cannot be the same character"))

(define-condition incompatible-exponent-allowed-error () ()
  (:report "EXPONENT-ALLOWED cannot share characters with DECIMAL-MARK or GROUP-SEPARATOR"))

(defun parse-decimal (string &key (start 0)
			       end
			       (sign-allowed t)
			       (decimal-mark #\.)
			       group-separator
			       exponent-allowed
			       junk-allowed)
  "PARSE-DECIMAL parses a decimal number from the substring of STRING
delimited by START and END. Optional leading and trailing whitespace[1] is
ignored. The character parameters DECIMAL-MARK and GROUP-SEPARATOR provide
support for different (typically cultural) numerical conventions. For
convenience, a GROUP-SEPARATOR of T will be treated as the comma
character. The boolean SIGN-ALLOWED controls signage. EXPONENT-ALLOWED is a
boolean or a sequence controlling exponent notation. Exponent notation follows
the syntax for CL floats, with exception that the exponent marker must be 'e'
or 'E' when EXPONENT-ALLOWED is T, or the exponent marker must be CHAR= to
some element of EXPONENT-ALLOWED when EXPONENT-ALLOWED is a
sequence. JUNK-ALLOWED behaves as it does in PARSE-INTEGER. PARSE-DECIMAL
returns two values: the parsed number of type DECIMAL, followed by the index
of STRING where the parse terminated. 
Example: (parse-decimal \"1,205.42\" :group-separator #\\,) => 60271/50, 8"
  (check-type string string)
  (check-type start integer)
  (check-type end (or null integer))
  (check-type sign-allowed boolean)
  (check-type decimal-mark character)
  (check-type group-separator (or boolean character))
  (check-type exponent-allowed (or boolean sequence))
  (check-type junk-allowed boolean)
  (let ((strlen (length string)))
    (when (null end)
      (setf end strlen))
    (when (eq t group-separator) 
      (setf group-separator #\,))
    (when (eq t exponent-allowed)
      (setf exponent-allowed "eE"))
    (cond ((zerop strlen)
	   (if junk-allowed
	       (return-from parse-decimal (values nil start))
	       (error 'parse-error)))
	  ((not (and (<= 0 start end strlen) (< start end)))
	   (error 'out-of-bounds-error))
	  ((and decimal-mark 
		group-separator 
		(char= decimal-mark group-separator))
	   (error 'incompatible-mark-and-separator-error))
	  (exponent-allowed
	   (when (or (find decimal-mark exponent-allowed :test #'char=)
		     (and group-separator 
			  (find group-separator exponent-allowed :test #'char=)))
	     (error 'incompatible-exponent-allowed-error)))))
  ;; Do the actual parsing. The enclosed tagbody acts as a state machine. At
  ;; each state we either loop within that state, or advance forward to the
  ;; next applicable state (via fall-through or explicit GO).
  (let ((numerator 0)
	(denominator 1)
	(sign 1)
	(exponent 0)
	(exponent-sign 1)
	valid
	(char-code-0 (char-code #\0))
	(i (1- start))
	char_i)
    (labels ((return-valid ()
	       (return-from parse-decimal (values (* sign 
						     (/ numerator denominator)
						     (expt 10 (* exponent-sign exponent)))
						  i)))
	     (return-invalid ()
	       (if junk-allowed
		   (return-from parse-decimal (values nil i))
		   (error 'parse-error)))
	     (read-next () 
	       (incf i)
	       (when (= i end)
		 (if valid
		     (return-valid)
		     (return-invalid)))
	       (setf char_i (elt string i))))
      (tagbody
	 (read-next)
       maybe-leading-whitespace
	 (when (whitespace-1-p char_i)
	   (read-next)
	   (go maybe-leading-whitespace))
       maybe-sign
	 (when sign-allowed
	   (cond
	     ((char= #\- char_i)
	      (setf sign -1)
	      (read-next))
	     ((char= #\+ char_i)
	      (read-next))))
       maybe-left-digit-or-group-separator
	 (cond
	   ((char<= #\0 char_i #\9)
	    (setf numerator (+ (* numerator 10) (- (char-code char_i) char-code-0)))
	    (setf valid t)
	    (read-next)
	    (go maybe-left-digit-or-group-separator))
	   ((and group-separator (char= group-separator char_i))
	    (read-next)
	    (go maybe-left-digit-or-group-separator)))
       maybe-decimal-mark
	 (cond
	   ((char= decimal-mark char_i)
	    (read-next)
	    (if valid ; if we have seen a left digit
		(go maybe-right-digit)
		(go right-digit)))
	   (t (if valid ; if we have seen a left digit
		  (go maybe-exponent-marker)
		  (return-invalid))))
       right-digit
	 (cond
	   ((char<= #\0 char_i #\9)
	    (setf numerator (+ (* numerator 10) (- (char-code char_i) char-code-0)))
	    (setf denominator (* denominator 10))
	    (setf valid t)
	    (read-next))
	   (t (return-invalid)))
       maybe-right-digit
	 (when (char<= #\0 char_i #\9)
	   (setf numerator (+ (* numerator 10) (- (char-code char_i) char-code-0)))
	   (setf denominator (* denominator 10))
	   (read-next)
	   (go maybe-right-digit))
       maybe-exponent-marker
	 (cond 
	   ((find char_i exponent-allowed)
	    (setf valid nil) ; invalid until we finish parsing the exponent
	    (read-next))
	   (t (go maybe-trailing-whitespace)))
       maybe-exponent-sign
	 (cond
	   ((char= #\- char_i)
	    (setf exponent-sign -1)
	    (read-next))
	   ((char= #\+ char_i)
	    (read-next)))
       exponent-digit
	 (cond
	   ((char<= #\0 char_i #\9)
	    (setf exponent (+ (* exponent 10) (- (char-code char_i) char-code-0)))
	    (setf valid t)
	    (read-next))
	   (t (return-invalid)))
       maybe-exponent-digit
	 (when (char<= #\0 char_i #\9)
	   (setf exponent (+ (* exponent 10) (- (char-code char_i) char-code-0)))
	   (read-next)
	   (go maybe-exponent-digit))
       maybe-trailing-whitespace
	 (cond
	   ((whitespace-1-p char_i)
	    (read-next)
	    (go maybe-leading-whitespace))
	   (t (return-invalid)))))))

(defun count-digits (integer)
  (check-type integer integer)
  (setf integer (abs integer))
  (if (zerop integer)
      1
      ;; Could use log instead but it might not be safe in every case
      (do ((i 0 (1+ i)))
	  ((< integer 1) i)
	(setf integer (/ integer 10)))))

(defun find-multiplier (d)
  "Given an integer, d, of the form 2^m*5^n, compute c such that c*d = c*(2^m*5^n) = 10^max(m,n).
Returns c and max(m,n). See http://wukix.com/lisp-decimals#theory for background."
  (let (2^m 5^n	m n)
    (setf 2^m (ash (1+ (logxor d (1- d))) -1))
    (setf 5^n (/ d 2^m))
    (setf m (1- (integer-length 2^m)))
    (setf n (round (log 5^n 5)))
    (if (> m n)
	(values (expt 5 (- m n)) m)
	(values (expt 2 (- n m)) n))))

(defun integer-to-digit-string (integer)
  "Converts an integer into a base 10 string. The sign is ignored."
  (check-type integer integer)
  (setf integer (abs integer))
  (if (zerop integer)
      "0"
      (let* ((digit-count (do ((x integer (truncate x 10))
			       (i 0 (1+ i)))
			      ((zerop x) i)))
	     (str (make-string digit-count))
	     (char-code-0 (char-code #\0))
	     remainder)
	(do ((i (1- digit-count) (1- i)))
	    ((minusp i) str)
	  (multiple-value-setq (integer remainder)
	    (truncate integer 10))	  
	  (setf (elt str i) (code-char (+ char-code-0 remainder)))))))

(define-condition disallowed-loss-of-precision () ()
  (:report "Attempted print operation would lose precision. See *print-precision-loss*"))

(defun decimal-to-digits (number &optional position relativep)
  (when (and position relativep) 
    (assert (plusp position)))
  (multiple-value-bind (multiplier denominator-power)
      (find-multiplier (denominator number))
    (let* ((all-digits (* multiplier (numerator number)))
	   (digit-count (count-digits all-digits))
	   (decimal-mark-index (- digit-count denominator-power)))
      (if position
	  (if relativep
	      (if (plusp (+ position decimal-mark-index))
		  (let ((new-digit-count (min digit-count
					      (+ (min position (+ (max 0 (- decimal-mark-index)) 
								  digit-count)) 
						 (min 0 decimal-mark-index)))))
		    (when (/= new-digit-count digit-count)
		      (ecase *print-precision-loss* 
			(:error
			 (error 'disallowed-loss-of-precision))
			(:truncate
			 (setf all-digits (truncate all-digits (expt 10 (- digit-count new-digit-count)))))
			(:round
			 (multiple-value-bind (rounded-all-digits remainder)
			     (round all-digits (expt 10 (- digit-count new-digit-count)))
			   (when (and (minusp remainder) 
				      (/= digit-count (count-digits (- all-digits remainder))))
			     (incf decimal-mark-index))
			   (setf all-digits rounded-all-digits))))
		      (when (zerop all-digits)
			(setf decimal-mark-index 0)))
		    (values decimal-mark-index (integer-to-digit-string all-digits)))
		  (values 0 "0"))
	      (if (plusp (- (max 0 decimal-mark-index) position))
		  (let ((new-digit-count (min digit-count 
					      (- decimal-mark-index position))))
		    (when (/= new-digit-count digit-count)
		      (ecase *print-precision-loss* 
			(:error
			 (error 'disallowed-loss-of-precision))
			(:truncate
			 (setf all-digits (truncate all-digits (expt 10 (- digit-count new-digit-count)))))
			(:round
			 (multiple-value-bind (rounded-all-digits remainder)
			     (round all-digits (expt 10 (- digit-count new-digit-count)))
			   (when (and (minusp remainder) 
				      (/= digit-count (count-digits (- all-digits remainder))))
			     (incf decimal-mark-index))
			   (setf all-digits rounded-all-digits))))
		      (when (zerop all-digits)
			(setf decimal-mark-index 0)))
		    (values decimal-mark-index (integer-to-digit-string all-digits)))
		  (values position "0")))
	  (values decimal-mark-index (integer-to-digit-string all-digits))))))

(defun decimal-to-string (x &optional width fdigits scale fmin)
  (check-type x decimal)
  (setf x (abs x))
  (multiple-value-bind (e string)
      (if fdigits
          (decimal-to-digits x (min (- (+ fdigits (or scale 0)))
				    (- (or fmin 0))))
          (if (and width (> width 1))
              (let ((w (multiple-value-list
                        (decimal-to-digits x
					   (max 1
						(+ (1- width)
						   (if (and scale (minusp scale))
						       scale 0)))
					   t)))
                    (f (multiple-value-list
                        (decimal-to-digits x (- (+ (or fmin 0)
						   (if scale scale 0)))))))
                (cond
                  ((>= (length (cadr w)) (length (cadr f)))
                   (values-list w))
                  (t (values-list f))))
              (decimal-to-digits x)))
    (let ((e (+ e (or scale 0)))
          (stream (make-string-output-stream)))
      (if (plusp e)
          (progn
            (write-string string stream :end (min (length string) e))
            (dotimes (i (- e (length string)))
              (write-char #\0 stream))
            (write-char *print-decimal-mark* stream)
            (write-string string stream :start (min (length string) e))
            (when fdigits
              (dotimes (i (- fdigits
                             (- (length string)
                                (min (length string) e))))
                (write-char #\0 stream))))
          (progn
            (write-string (string *print-decimal-mark*) stream)
            (dotimes (i (- e))
              (write-char #\0 stream))
            (write-string string stream)
            (when fdigits
              (dotimes (i (+ fdigits e (- (length string))))
                (write-char #\0 stream)))))
      (let ((string (get-output-stream-string stream)))
        (values string (length string)
                (char= (char string 0) *print-decimal-mark*)
                (char= (char string (1- (length string))) *print-decimal-mark*)
                (position *print-decimal-mark* string))))))

(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (let ((spaceleft w))
    (when (and w (or atsign (minusp number)))
      (decf spaceleft))
    (multiple-value-bind (str len lpoint tpoint)
	(decimal-to-string (abs number) spaceleft d k)
      ;; if caller specifically requested no fraction digits, suppress the
      ;; optional trailing zero
      (when (and d (zerop d))
	(setq tpoint nil))
      (when w
	(decf spaceleft len)
	;; optional leading zero
	(when lpoint
	  (if (or (> spaceleft 0) tpoint) ;force at least one digit
	      (decf spaceleft)
	      (setq lpoint nil)))
	;; optional trailing zero
	(when tpoint
	  (if (> spaceleft 0)
	      (decf spaceleft)
	      (setq tpoint nil))))
      (cond ((and w (< spaceleft 0) ovf)
	     ;; field width overflow
	     (dotimes (i w)
	       (write-char ovf stream))
	     t)
	    (t
	     (when w
	       (dotimes (i spaceleft)
		 (write-char pad stream)))
	     (if (minusp number)
		 (write-char #\- stream)
		 (when atsign
		   (write-char #\+ stream)))
	     (when lpoint
	       (write-char #\0 stream))
	     (write-string str stream)
	     (when tpoint
	       (write-char #\0 stream))
	     nil)))))

(defun $ (stream decimal colon atsign &optional d n w pad)
  "e.g. (format t \"~/wu-decimal:$/\" #$3.14)"
  (check-type decimal decimal)
  (when (null d) (setf d 2))
  (when (null n) (setf n 1))
  (when (null w) (setf w 0))
  (let* ((signstr (if (minusp decimal)
		      "-"
		      (if atsign "+" "")))
	 (signlen (length signstr)))
    (multiple-value-bind (str strlen ig2 ig3 pointplace)
	(decimal-to-string decimal nil d nil)
      (declare (ignore ig2 ig3 strlen))
      (when colon
	(write-string signstr stream))
      (dotimes (i (- w signlen (max n pointplace) 1 d))
	(write-char pad stream))
      (unless colon
	(write-string signstr stream))
      (dotimes (i (- n pointplace))
	(write-char #\0 stream))
      (write-string str stream))))

(defun f (stream number colon atsign &optional w d k ovf pad)
  "e.g. (format t \"~/wu-decimal:F/\" #$3.14)"
  (declare (ignore colon))
  (unless pad
    (setf pad #\Space))
  (format-fixed-aux stream number w d k ovf pad atsign))
