;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok-Library - Octets and octet vectors primitives
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See also acl-compat.lisp: octets-to-string and string-to-octets

(in-package :ystok.library)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(vector octet))
(deftype simple-octet-vector () '(simple-array octet (*)))

(defmacro make-octet-vector (size &rest args)
 ;;; Return a simple-octet-vector or octet-vector if fill-pointer or adjustable specified
  ;; Args: Key parmaters initial-element, initial-contents, fill-pointer, adjustable
  `(make-array ,size :element-type 'octet ,@args))

(declaim (inline octet-vector-p))
(defun octet-vector-p (object)
  (and (vectorp object) (equal (array-element-type object) '(unsigned-byte 8))))

;;; Converting to/from integer
;;; CAUTION: (declare (optimize (hcl:fixnum-safety 0))) dangerous for bignums!

(defun octets-to-signed (octets &optional (start 0) end)
 ;;; Assemble a signed integer, last octet determines its sign
  ;; Args: start, end If delimit an empty subsequence, 0 is returned
  (declare #-debug (optimize (debug 0) (speed 3) (safety 0))
           (fixnum start))
  (do* ((end (or end (length octets)))
        (acc (if (and (< start end) (logtest (the octet (aref octets (1- end))) #x80))
                 -1 0))					; initialize for correct sign
        (i start (1+ i))
        (j 0 (+ j 8)))
       ((>= i end)
        acc)
    (declare (fixnum end i j) (type integer acc))
    (setq acc (dpb (aref octets i) (byte 8 j) acc))))

(defun octets-to-unsigned (octets &optional (start 0) end)
 ;;; Assemble an (unsigned-byte *) value from the given octet vector
  ;; Args: octets Octet vector or NIL
  ;; Value: 0 if octets is NIL or start >= end.
  (declare #-debug (optimize (debug 0) (speed 3) (safety 0))
           (fixnum start))
  (do* ((end (or end (length octets)))
        (acc 0)
        (i start (1+ i))
        (j 0 (+ j 8)))
       ((>= i end)
        acc)
    (declare (fixnum end i j) (type unsigned-byte acc))
    (setq acc (logior acc (ash (aref octets i) j)))))

(defun integer-to-octets (n &key length place
                                 (start (if (and place (array-has-fill-pointer-p place))
                                            (fill-pointer place)
                                            0)))
 ;;; Convert the signed integer to octets in little endian mode.
  ;; Args: n      Arbitrary integer.
  ;;       length If is integer, specifices the exact integer length in octets.
  ;;              Otherwise has the meaning "signed" (generalize boolean):
  ;;                True = signed,
  ;;                False = unsigned,
  ;;		  and the actual length is computed by means of (integer-length n).
  ;;       place  NIL or an existing octet vector;
  ;;              if it has a fill-pointer, it must also be adjustable.
  ;;       start  Starting position in place;
  ;;              the dufault is either the fill-pointer of place or 0.
  ;; Value: A new simple-octet-vector of the length (greater than 1 if defaulted)
  ;;        or the place, maybe adjusted (by 16 bytes increment).
  ;; NB: If the place is not adjustable and there is not enough space,
  ;;     the function signals an error.
  ;; Q: Does little endian fit all cases?
  (declare #-debug (optimize (debug 0) (speed 3) (safety 0))  ;(hcl:fixnum-safety 0))
           (fixnum start))
  (do* ((end (+ start (if (integerp length)
                          length
                          (ceiling (if length
                                       (1+ (integer-length n))	; signed - 1 bit for sigh
                                       (progn			; unsigne - no sign bit
                                         (when (minusp n)
                                           (warn
                       "Negative integer ~S passed to integer-to-octets in unsigned mode."
                                                  n))
                                         (max (integer-length n) 1)))	; at least one byte
                                   8))))
        (octets (cond ((null place)
                       (make-octet-vector end))
                      ((<= end (length place))
                       place)
                      ((array-has-fill-pointer-p place)
                       (let ((size (array-total-size place)))
                         (if (<= end size)
                             (progn (setf (fill-pointer place) end)
                               place)
                             (adjust-array place (+ size 16) :fill-pointer end))))
                      ((adjustable-array-p place)
                       (adjust-array place (max (+ (length place) 16) end)))
                      (t (error "Cannot push integer ~S into octets: size to small." n))))
        (i start (1+ i))
        (j 0 (+ j 8)))
       ((>= i end)
        octets)
      (declare (fixnum end i j))
      (setf (aref octets i) (ldb (byte 8 j) n))))

(defun octet-vector (&rest args)
 ;;; Return a vector of octets made from all of the arguments given.
  ;; Args: args List with each element is
  ;;            - either arbitrary non-negative integer (i.e. of type unsigned-byte),
  ;;            - or cons (integer . octet-length).
  ;; Value: Specialized but non-simple vector (adjustable, with a fill-pointer). 
  ;; NB: This function is similar to the Common Lisp vector function.
  ;;     In simple, the result is computed as
  ;;       (make-octet-vector (length args) :initial-contents args)
  ;;     But each of the args can result in several bytes as of little endian.
  ;;     For non-negative arguments, a simple integer form can be used.
  ;;     Otherwise, the second dotted pair form should be used to ensure correct length.
  (let ((vector (make-octet-vector 16 :fill-pointer 0 :adjustable t)))
    (dolist (arg args)
      (integer-to-octets (etypecase arg
                           (integer arg)
                           (cons (car arg)))
                         :length (if (consp arg) (cdr arg) nil)		; nil = unsigned
                         :place vector))
    (shrink-vector vector (fill-pointer vector))))

;;; Coverting to/from string

(defun hex-string-to-octets (string &optional errorp)
 ;;; String to octet-vector converter (from cry-basics.lisp)
  ;; Args: string String or NIL.
  ;; Value: simple-octet-vector
  (declare (optimize (debug 0) (speed 3) (safety 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (do* ((string (remove-if-not #'hex-digit-char-p string))
        (length (length string))
        (octet-length (the fixnum (ceiling length 2)))
        (octets (make-octet-vector octet-length))
        (start 0 end)
        (end (+ start 2) (+ start 2))
        (j 0 (1+ j)))
       ((< length end)
        (cond ((>= start length))
              (errorp
               (error (if (stringp errorp) errorp "Hex string junk found: ~s")
                      (subseq string start)))
              (t
               (setf (aref octets j) (hex-digit-char-p (char string start)))))
        octets)
    (declare (fixnum length octet-length start end j))
    (let ((code (parse-integer string :start start :end end
                               :radix 16 :junk-allowed (not errorp))))
      (setf (aref octets j) (or code 0)))))

(defmethod object-hex-string ((vector vector) &optional char interval bit-count)
 ;;; Args: vector Specialize vector with element type octet or (unsigned-byte 16)
  (let ((length (length vector))
        (j -1)
        string)
    (declare (fixnum length j))
    (case (orf bit-count
               (let ((array-element-type (array-element-type vector)))
                 (and (consp array-element-type)
                      (eq (first array-element-type) 'unsigned-byte))
                 (second array-element-type)))
      ((8 nil)				; (vector (unsigned-byte 8))
       (setq string (make-string
                     (if char 
                         (+ #1=(ash length 1)
                            (if (< 0 length)
                                (truncate (1- length) (orf interval
                                                           (if (eql bit-count 16) 2 1)))
                                0))
                            #1#)
                         :element-type 'base-char))
       (dotimes (i length)
         (declare (fixnum i))
         (let ((byte (aref vector i)))
           (declare (type (unsigned-byte 8) byte))
           (when (and char (< 0 i) (= (the fixnum (rem i interval)) 0))
             (setf (schar string (incf j)) char))
           (setf (schar string (incf j)) (hex-digit-char (ash byte -4))
                 (schar string (incf j)) (hex-digit-char (logand byte #x0F)) ))))
      (16					; (vector (unsigned-byte 16))
       (setq string (make-string (ash length 2) :element-type 'base-char))
       (dotimes (i length)
         (declare (fixnum i))
         (let ((byte (aref vector i)))
           (declare (type (unsigned-byte 16) byte))
           (setf (schar string (incf j)) (hex-digit-char (ldb (byte 4 12) byte)))
           (setf (schar string (incf j)) (hex-digit-char (ldb (byte 4 8) byte)))
           (setf (schar string (incf j)) (hex-digit-char (ldb (byte 4 4) byte)))
           (setf (schar string (incf j)) (hex-digit-char (ldb (byte 4 0) byte))) ))))
    string))

(defun pad-octets (octets &key (length 32) padding)
 ;;; Pad to the right with zeros or with the contents of the padding specified
  ;; or truncate from the right.
  ;; Args: octets  Octet vector, simple or not (or even NIL!)
  ;;       lenght  Desired octet length of the result.
  ;;       padding NIL, octet vector, or any sequence containing only integers in 0..255
  ;; Value: May be eq to the octets argument.
  (declare #-debug (optimize (debug 0) (speed 3) (safety 0)
                             #+lisworks (hcl:fixnum-safety 0))
           (fixnum length))
  (let ((end (length octets)))
    (declare (fixnum end))
    (cond ((< end length)
           (let ((result (make-octet-vector length :initial-element 0)))
             (declare (type simple-octet-vector result))
             (when (< 0 end)
               (locally (declare (type octet-vector octets))
                 (replace result octets)))
             (when (< end (length padding))
               (locally (declare (type octet-vector padding))
                 (replace result padding :start1 end :start2 end)))
             result))
          ((< length end)
           (locally (declare (type octet-vector octets))
             (subseq octets 0 length)))
          (t octets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  BITWISE OPERATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Args: arg1, arg2  May be not simple arrays

(defun octet-and (arg1 arg2 &optional dest)
  (declare (type #1=(vector octet) arg1 arg2)
   #-debug (optimize (speed 3) (space 0) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (let* ((length (length arg1))
         (result (cond ((null dest) (make-octet-vector length))
                       ((eq dest t) arg1)
                       ((and (typep dest '#1#) (= (length dest) length)) dest)
                       (t (error "For octets-and, bad destination argument: ~s" dest)))))
    (declare (fixnum length)
             (type #1# result))
    (dotimes (i length)
      (declare (fixnum i))
      (setf (aref result i) (the octet (logand (aref arg1 i) (aref arg2 i)))))
    result))

;;; Error detection

(defun crc-8 (octets polynom initial-value &optional (start 0) end)
 ;;; One byte Cyclic Redundancy Check (CRC) 
  ;; Args: polynom       Octet, e.g. #b00011101 represents x^8 + x^4 + x^3 + x^2 + 1
  ;;       initial-value Preset
  ;; Value: octet.
  (declare (type octet-vector octets)
           (fixnum polynom initial-value)
           (fixnum start)
   #-debug (optimize (debug 0) (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (do* ((end (the fixnum (or end (length octets))))
        (i start (1+ i)))
       ((<= end i)
        (the octet (ldb (byte 8 0) initial-value)))
    (declare (fixnum end i))
    (setq initial-value (the fixnum (logxor initial-value (aref octets i))))
    (dotimes (j 8)
      (declare (fixnum j) (ignorable j))
      (setq initial-value (ldb (byte 8 0)
                               (if (logtest #x80 initial-value)
                                   (logxor (ash initial-value 1) polynom) ; set MS bit to 0
                                   (ash initial-value 1)))))))

#+unused
(defun lrc (octets &optional (start 0) end)
 ;;; One byte Longitudinal Redundancy Checksum (LRC)
  ;; Value: octet.
  ;; NB: wikipedia.org present another algorithm.
  (declare (type octet-vector octets)
           (fixnum start)
   #-debug (optimize (speed 3) (space 0) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (do* ((end (the fixnum (or end (length octets))))
        (acc 0)
        (i start (1+ i)))
       ((<= end i)
        (ldb (byte 8 0) acc))			; wikipedia:(1+ (logxor acc #xFF))
    (declare (fixnum end acc i))
    (setq acc (logxor acc (aref octets i)))))	; wikipedia:(ldb . (+ acc (aref octets i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;  READING OCTET VECTORS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following kinds of syntax are supported:
;;  #V"hh ..."  - from hex string
;;  #nVinteger  - from integer number
;;  #nV(i1 ...) - from list of integers la #()
;; The n argument provides the exact octet-length.
(set-dispatch-macro-character #\# #\V
   (lambda (stream subchar arg)			; (char= (peek-char nil stream) #\")
     (declare (ignore subchar))
     (let* (;(*read-base* 16)
            (object (read stream t nil t)))
       (cond ((stringp object)
              (hex-string-to-octets object))
             ((listp object)
              (cond ((null arg)				; elements can be any integers
                     (apply #'octet-vector object))
                    ((= (length object) arg)		; elements must be of type octet
                     (make-octet-vector arg :initial-contents object))
                    ((< (length object) arg)			; fill the rest of octets
                     (let ((octets (make-octet-vector arg
                                    :initial-element (or (first (last object)) 0))))
                       (if object
                           (replace octets object)
                           octets)))
                    (t
                     #+lispworks
                     (error 'conditions:simple-reader-error
                            :format-control "Vector longer than specified length: #~D~S."
                            :format-arguments (list arg object))
                     #-lispworks
                     (error "Vector longer than specified length: #~D~S." arg object))))
             ((integerp object)
              (integer-to-octets object :length arg))
             (t
              #+lispworks
              (error 'conditions:simple-reader-error
                 :format-control "Bad token after #V, a string of hex digits or list of bytes is expected.")
              #-lispworks
              (error "Bad token after #V, a string of hex digits or list of bytes is expected."))))))

;;;;;;;;;;;;;;;;;;; STRING-TO-OCTETS, OCTETS-TO-STRING  ;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation Notes
;;;  1)	External formats :UTF-8 and :ASCII are supported for all Lisp implementations.
;;;     Other implementation-specific values are accepted only on LispWorks and SBCL.
;;;  2)	We only deal with :ascii, code page based, and (reasonable subset of)
;;;	Unicode UTF-16 external formats.
;;;  3)	On LispWork 4.x, Newline<->CrLf translation is only supported for external
;;;     formats based on code page.
;;;  4) The followng Allegro parameters are not supported:
;;;     mb-vector, make-mb-vector?
;;;     string make-string? truncate string-start string-end

#-sbcl
(defun string-to-octets (string &key (start 0) end (external-format :default)
                                     (null-terminate t))
                                     ;mb-vector make-mb-vector?
 ;;; Returns a Lisp vector and the number of bytes copied.
  ;; Args: string          May not be simple, i.e. may have a fill-pointer.
  ;;       external-format :utf-8, :ascii
  ;;			   	    Work for all Lisp implementations,
  ;;			   :default Is only valid in LispWorks (for desktop applications),
  ;;			   other    LispWorks- or SBCL-specific.
  ;;       null-terminate  CAUTION: In flexi-streams:string-to-octets defaults to NIL.
  ;; Values: 1) Simple octet vector, i.e. of type (unsigned-byte 8).
  ;;         2) The number of bytes copied.
  (declare (type string string)
           (fixnum start)
           #-debug (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (unless end
    (setq end (length string)))
  (case (yl:first-or-self external-format)
    (:utf-8
     ;; Special treatment of UTF-8, no CRLF translation
     (let* ((to-index -1)
            (crlf (and (consp external-format)
                       (eq (getf (rest external-format) :EOL-STYLE) :crlf)))
            (needed-length (+ (ash (- (the fixnum end) start) 4)	; 6?
                              (if crlf (count #\Newline string) 0)
                              (if null-terminate 1 0)))
            (octets (make-octet-vector needed-length)))
                    ;(cond ((and mb-vector (>= (length mb-vector) needed-length))mb-vector)
                    ;      ((or (not mb-vector) make-mb-vector?)
                    ;	    (make-octet-vector needed-length)	;:initial-element 0
                    ;(t (error "Was given a vector of length ~A, ~but needed at least ~A."
                    ;          (length mb-vector) needed-length)))
       (declare (type simple-octet-vector octets)
                (fixnum to-index))
       (do ((from-index start (1+ from-index)))
           ((>= from-index (the fixnum end)))
         (declare (fixnum from-index))
         (let* ((char (aref string from-index))
                (code (char-code char)))
           (tagbody
            (cond ((< code #x80)
                   (if (and (char= char #\Newline) crlf)
                       (setf (aref octets (incf to-index)) 13
                             (aref octets (incf to-index)) 10)
                       (setf (aref octets (incf to-index)) code))
                   (go zero))
                  ((< code #x800)
                   (setf (aref octets (incf to-index))
                         (the fixnum (logior #b11000000 (the fixnum (ash code -6)))))
                   (go one))
                  ((< code #x10000)
                   (setf (aref octets (incf to-index))
                         (the fixnum (logior #b11100000 (the fixnum (ash code -12)))))
                   (go two))
                  (t
                   (setf (aref octets (incf to-index))
                         (the fixnum (logior #b11110000 (the fixnum (ash code -18)))))))
            (setf (aref octets (incf to-index))
                  (the fixnum (logior #b10000000
                                      (the fixnum (logand #b00111111
                                                          (the fixnum (ash code -12)))))))
            two
            (setf (aref octets (incf to-index))
                  (the fixnum (logior #b10000000
                                      (the fixnum (logand #b00111111
                                                          (the fixnum (ash code -6)))))))
            one
            (setf (aref octets (incf to-index))
                  (the fixnum (logior #b10000000 (the fixnum (logand #b00111111 code)))))
            zero)))
       (when null-terminate
         (setf (aref octets (incf to-index)) 0))
       (incf to-index)
       (return-from string-to-octets
         (values (yl:shrink-vector octets to-index)
                 to-index))))
    (:ascii
     (let* ((crlf (and (consp external-format)
                       (eq (getf (rest external-format) :eol-style) :crlf)))
            (needed-length (+ (- (the fixnum end) start)
                              (if crlf (count #\Newline string) 0)
                              (if null-terminate 1 0)))
            (octets (make-octet-vector needed-length))
            (to-index -1))
       (declare (type simple-octet-vector octets)
                (fixnum needed-length to-index))
       (do ((from-index start (1+ from-index)))
           ((>= from-index (the fixnum end)))
         (let ((char (char string from-index)))
           (if (and (char= char #\Newline) crlf)
               (setf (aref octets (incf to-index)) 13
                     (aref octets (incf to-index)) 10)
               (setf (aref octets (incf to-index)) (char-code char)))))
       (when null-terminate
         (setf (aref octets (incf to-index)) 0))
       (return-from string-to-octets
         (values octets (1+ to-index)))))
    #+lispworks
    (:default
     (setq external-format (if (lw:text-string-p string)
                               #+win32 win32:*multibyte-code-page-ef*
                               #-win32 stream::*default-external-format*)))
                           ;(if (base-string-p string) :latin-1 :unicode)
    #-lispworks
    (otherwise
     (error "For ~S, string-to-octets is implemented only for UTF-8."
            (lisp-implementation-type))))

  #+lispworks
  (let* ((crlf (and (consp external-format)
                    (eq (EF::EF-PARAMS-EOL-STYLE (rest external-format)) :crlf)))
         (code-page (when (consp external-format)
                      (getf (rest external-format) :id))) ; LW4: ef::ef-coded-character-set
         (to-index -1)
         (needed-length (ash (+ (- (the fixnum end) start) (if null-terminate 1 0))
                             (if (or (eq external-format :unicode) crlf) 1 0)))
         (octets (make-octet-vector needed-length)))
                 ;(cond ((and mb-vector (>= (length mb-vector) needed-length)) mb-vector)
                 ;      ((or (not mb-vector) make-mb-vector?)
                 ;       (make make-octet-vector needed-length) ;:initial-element 0
                 ;    (t (error "Was given a vector of length ~A, ~but needed at least ~A."
                 ;                   (length mb-vector) needed-length)))))
    (declare (type simple-octet-vector octets)
             (fixnum to-index))
    (do ((from-index start (1+ from-index)))
        ((>= from-index (the fixnum end)))
      (declare (fixnum from-index))
      (let ((char (aref string from-index)))
        (cond ((eq external-format :unicode)
               (let ((char-code (char-code char)))
 #+little-endian (setf (aref octets (incf to-index)) (ldb (byte 8 0) char-code)
                       (aref octets (incf to-index)) (ldb (byte 8 8) char-code))
 #-little-endian (setf (aref octets (incf to-index)) (ldb (byte 8 8) char-code)
                       (aref octets (incf to-index)) (ldb (byte 8 0) char-code))))
              (code-page
               (if (and (char= char #\Newline) crlf)
                   (setf (aref octets (incf to-index)) 13
                         (aref octets (incf to-index)) 10)
                   (setf (aref octets (incf to-index))
                         (ef:char-external-code char code-page))))
              (t
               (setf (aref octets (incf to-index)) (char-code char))) )))
    (when null-terminate
      (setf (aref octets (incf to-index)) 0)
      (when (eq external-format :unicode)
        (setf (aref octets (incf to-index)) 0)))
    (incf to-index)
    (values (if crlf (yl:shrink-vector octets to-index) octets)
            to-index)))

#-sbcl
(defun octets-to-string (octets &key (start 0) end (external-format :default))
                                     ;string make-string? truncate string-start string-end
 ;;; Return a string from the octets sequence.
  ;; Args: octets  Specialized vector of element-type (unsigned-byte 8)
  ;;               Must be simple - a fill-pointer is not allowed!
  ;;       end     If is NIL, scan to the end of the vector or until the terminating
  ;;		   octet 0 is encountered (in the latter case, this octet 0 is not read).
  ;;               If not null, #\Null chars may appear in the result string.
  ;; Values: 1) Simple string.
  ;;	     2) The number of characters copied.
  ;;	     3) The number of octets used.
  ;; NB: LispWorks implementation of ef:decode-external-string is rather inefficient
  ;;     versions prior 5.0.1.
  (declare (type simple-octet-vector octets)
           (fixnum start)
           #-debug (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (unless end
    (setq end (or (position 0 octets :start start) (length octets))))
  (case (yl:first-or-self external-format)
    (:utf-8
     ;; Based on function reinterpret-string-as-utf8 by Raymond Wiker
     (let* ((crlf (and (consp external-format)
                       (eq (getf (rest external-format) :eol-style) :crlf)))
            (string
             (with-output-to-string (stream nil :element-type #+lispworks 'lw:simple-char
                                                              #-lispworks 'character)
              (do ((chars-remaining 0)
                   (accumulator 0)
                   (end end)
                   (from-index start (1+ from-index)))
                  ((>= from-index end)
                   (unless (= chars-remaining 0)
                     (error "Unexpected end of UTF-8 sequence.")))
                (declare (fixnum chars-remaining accumulator end from-index))
                (let ((octet (aref octets from-index)))		; output-byte
                  (declare (type yl:octet octet))
                  (cond ((= chars-remaining 0)
                         (cond ((= (logand octet #b10000000) 0)
                                (if (and (= octet 13) crlf
                                         (< (1+ from-index) end)
                                         (= (the yl:octet (aref octets (1+ from-index))) 10))
                                    (progn (write-char #\Newline stream)
                                      (incf from-index))
                                    (write-char (code-char octet) stream)))
                               ((= (logand octet #b11100000) #b11000000)
                                (setq accumulator (logand octet #b00011111)
                                      chars-remaining 1))
                               ((= (logand octet #b11110000) #b11100000)
                                (setq accumulator (logand octet #b00001111)
                                      chars-remaining 2))
                               ((= (logand octet #b11111000) #b11110000)
                                (setq accumulator (logand octet #b00000111)
                                      chars-remaining 3))
                               ((= (logand octet #b11111100) #b11111000)
                                (setq accumulator (logand octet #b00000011)
                                      chars-remaining 4))
                               ((= (logand octet #b11100000) #b11111110)
                                (setq accumulator (logand octet #b00000001)
                                      chars-remaining 5))
                               (t
                                #1=(error "Invalid octet #x~2,'0x at position ~d in UTF-8 sequence."
                                          octet from-index))))
                       ((= (logand octet #b11000000) #b10000000)
                        (setq accumulator (logior (ash accumulator 6)
                                                  (logand octet #b00111111)))
                        (when (= (decf chars-remaining) 0)
                          (write-char (code-char accumulator) stream)))
                       (t #1#)))))))
       (declare (type simple-string string))
       (return-from octets-to-string
         (values string (length string) (- end start)))))
    (:ascii
     (let* ((crlf (and (consp external-format)
                       (eq (getf (rest external-format) :eol-style) :crlf)))
            (octet-length (- end start))
            (to-index -1)
            (string (make-string octet-length :element-type #+lispworks 'lw:simple-char
                                                            #-lispworks 'character)))
       (declare (fixnum octet-length to-index)
                (type #+lispworks lw:simple-text-string #-lispworks simple-string string))
       (do ((end end)
            (from-index start (1+ from-index)))
           ((>= from-index end))
         (declare (fixnum end from-index))
         (let ((code (aref octets from-index)))
           (declare (type yl:octet code))
           (if (and (= code 13) crlf
                    (< (1+ from-index) end)
                    (= (the yl:octet (aref octets (1+ from-index))) 10))
               (setf (#+lispworks lw:stchar #-lispworks schar
                      string (incf to-index)) #\Newline
                     from-index (1+ from-index))
               (setf (#+lispworks lw:stchar #-lispworks schar
                      string (incf to-index)) (code-char code)))))
       (incf to-index)
       (values (if crlf (yl:shrink-vector string to-index) string)
               to-index
               octet-length)))
    #+lispworks
    (:default
     (setq external-format #+win32 win32:*multibyte-code-page-ef*
                           #-win32 stream::*default-external-format*))
    #-lispworks
    (otherwise
     (error "For ~S, octets-to-string is implemented only for UTF-8."
            (lisp-implementation-type))))

  #+lispworks
  (let* ((crlf (and (consp external-format)
                    (eq (EF::EF-PARAMS-EOL-STYLE (rest external-format)) :crlf)))
         (code-page (when (consp external-format)
                      (getf (rest external-format) :id))) ; LW4: ef::ef-coded-character-set
         (octet-length (- end start))
         (to-index -1)
         (string (make-string (if (eq external-format :unicode)
                                  (ash octet-length -1)
                                  octet-length)
                              :element-type 'lw:simple-char)))
                                            ;(ef:external-format-type external-format)
    (declare (fixnum octet-length to-index)
             (type lw:simple-text-string string))
    (do ((end end)
         (from-index start (1+ from-index)))
        ((>= from-index end))
      (declare (fixnum end from-index))
      (let ((code (aref octets from-index)))
        (declare (type yl:octet code))
        (cond ((eq external-format :unicode)		; UCS-2
               (let ((char-code 0))
                 (declare (fixnum char-code))
 #+little-endian (setf (ldb (byte 8 0) char-code) code
                       (ldb (byte 8 8) char-code) (aref octets (incf from-index)))
 #-little-endian (setf (ldb (byte 8 8) char-code) code
                       (ldb (byte 8 0) char-code) (aref octets (incf from-index)))
                 (setf (lw:stchar string (incf to-index)) (code-char char-code))))
              (code-page
               (if (and (= code 13) crlf
                        (< (1+ from-index) end)
                        (= (the yl:octet (aref octets (1+ from-index))) 10))
                   (setf (lw:stchar string (incf to-index)) #\Newline
                         from-index (1+ from-index))
                   (setf (lw:stchar string (incf to-index))
                         (ef:find-external-char code code-page))))
              (t
               (setf (lw:stchar string (incf to-index)) (code-char code))) )))
    (incf to-index)
    (values (if crlf (yl:shrink-vector string to-index) string)
            to-index
            octet-length)))


#|;; This does not work in LW
(defparameter *octet-vector-print* t)

(defmethod print-object ((self vector) stream)
  (if (and (octet-vectorp self) *octet-vector-print*)
      (let ((length (length self))
            (*print-base* 16))
        (format stream "#dV(" length)
        (dotimes (i (length)) (princ (aref self i) stream))
        (write-char #\) stream))
      (call-next-method)))
|#