;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok Library - Locale-controlled parser/transformer for real numbers
;;; Copyright (c) 2003-2006 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Based on H.G.Backer's article in ACM LISP Pointers IV,2 (April-June)

(in-package :ystok.library)
(in-syntax ystok.meta:*meta-readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(digit ctoi sign) :ystok.library)

 (setf (fdefinition 'digit) (fdefinition 'digit-char-p)
       (fdefinition 'ctoi)  (fdefinition 'digit-char-p))
)

(defun sign (char)
  (declare (type character char) (optimize (speed 3) (debug 0) (safety 0)))
  (or (char= char #\+) (char= char #\-)))

(defun parse-number (string &key (decimal-separator *decimal-separator*)
                                 (start 0) end junk-allowed)
 ;;; Relies on the CL reader when encounters a number with exponent.
  ;; Utilize both null tests on local variables in addition to the standard
  ;; match predicates in this program. For example, the test {!id which checks
  ;; for the existence of either integer or fraction digits, must succeed
  ;; before the exponent marker can be scanned.
  ;;
  ;; Preserving the characters that were looked at but not used requires
  ;; additional work, because CL streams support only 1 character lookahead,
  ;; yet many CL parsing tasks require up to 3 lookahead characters.
  (declare (type string string))
  (flet ((decimal-separator (char)
           (declare (type character char) (optimize (safety 0)))
           (eql char decimal-separator))
         (expmarker (char)
           (declare (type character char) (optimize (safety 0)))
           #+lispworks (sys::find-character\$simple-string "esfdlESFDL" 0 10 char)
           #-lispworks (find char "esfdlESFDL" :test #'char=)) )
   (let (ahead
         (is #\+) id (i 0)			; integral part
         dd (d 0) 				; ratio denominator
         ds ds-pos				; decimal separator
         fd (f 0) (f-denominator 1) ;(nf 0) 	; fractional part
         (m #\e) (es #\+) ed (e 0)		; mantissa marker and exponent
         position				; index of the first unparsable char
         (number nil))
    (ystok.meta:with-string-meta (string :start start :end end)
      (if (ystok.meta:match
	   [{[@(sign is) !(push is ahead)] []}				; scan sign
	    $[@(digit id) !(setq ahead nil i (+ (* i 10) (ctoi id)))]	; integer digits
	    {[!id #\/ !(push #\/ ahead)                                 ; ratio
	      $[@(digit dd) !(setq ahead nil d (+ (* d 10) (ctoi dd)))]]; denominator
	     [{[@(decimal-separator ds)					; dec.point
                !(setq ds-pos (1- ystok.meta:%index%))
                {!id !(push ds ahead)}			; to permit empty fractional
	        $[@(digit fd)						; fractional
	          !(setq ahead nil f (+ (* f 10) (ctoi fd))
                         f-denominator (* f-denominator 10))]] ;nf (1+ nf) 
	       []} 
	      {[{!id !fd} @(expmarker m) !(push m ahead)		; exp.marker
	        {[@(sign es) !(push es ahead)] []}			; exp.sign
	        $[@(digit ed) !(setq ahead nil e (+ (* e 10) (ctoi ed)))]] ; exp.digits
	          []}]}])
          (let ((sign (if (eql is #\-) -1 1)))
            ;(format t "~&>> match: ~s ~s ~s ~s ~s"
            ;    ahead ystok.meta:%source% ystok.meta:%index% ystok.meta:%end% ds-pos)
            (setq position (- ystok.meta:%index% (length ahead))
                  number
                   (cond (ed ;(make-float m sign i f nf (if (eql es #\-) (- e) e))
                          (if (or (null ds-pos) (eql decimal-separator #\.))
                              (read-from-string string nil nil
                                                :start start :end position)
                              (read-from-string	   ; replace custom separator by "."
                               (concatenate 'string
                                            (subseq string start ds-pos) "."
                                            (subseq string (1+ ds-pos) position))
                               nil nil)))
                         (fd (* sign (+ i (/ (float f) f-denominator)))) ;(expt 10 nf)
                         (dd (/ (* sign i) d))
                         (id (* sign i)))))
          ;; else doesn't match
          (setq position (- ystok.meta:%index% (length ahead))))
      (if (or junk-allowed (and number (eql position ystok.meta:%end%)))
          (values number position)
          #+(or lispworks sbcl)
          (error #+lispworks 'conditions::simple-parse-error
                 #+sbcl 'sb-int:simple-parse-error
                 :format-control #L"Illegal number format ~S~@[, char '~C'~]."
                 :format-arguments
                  (list* (subseq string start end)
                         (when (< ystok.meta:%index% ystok.meta:%end%)
                           (list (char ystok.meta:%source% ystok.meta:%index%)))))
          #-(or lispworks sbcl)
          (error 'parse-error))
    ))))


#||
(defun make-float (m sign i f nf ex)
  (format nil "<FLOAT :m ~S :sign ~A :i ~S :f ~S :nf ~S :ex ~S>" m sign i f nf ex))

-123.45e10
(parse-number "-123,45e-1.0" :junk-allowed t)
(parse-number "-3,à")
(parse-number "-,01" :junk-allowed t)
(parse-number "-123/45a" :junk-allowed t)

(defun parse-int (string &aux (s +1) d (n 0))
  (with-string-meta (string)
                    (and
                     (match
                      [{#\+ [#\- !(setq s -1)] []}
                            @(digit d) !(setq n (ctoi d))
                            $[@(digit d) !(setq n (+ (* n 10) (ctoi d)))]])
                     (* s n))))
(parse-int "-32")
||#
