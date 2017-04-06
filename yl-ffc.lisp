;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10;  -*-
;;; Ystok Library - Ystok-FFC bindings
;;; Copyright (c) 2003-2014 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is only loaded when the :ystok-ffc feature is present.

(in-package :ystok.library)

;;;;;;;;;;;;;;;;;;;;;;;;  CHARACTER CASE CONVERSION    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Win32 API uses the language driver for the current language selected
;;; by the user at setup or by using Control Panel.
;;; TODO: Implement on not-Windows platforms.

(declaim (inline locale-char-downcase locale-char-upcase)
         (ftype (function (character) character) locale-char-downcase locale-char-upcase))

(defun locale-char-downcase (char)
  (code-char (ffc::|CharLower| (char-code char))))

(defun locale-char-upcase (char)
  (code-char (ffc::|CharUpper| (char-code char))))

;;; Args: arg        String (simple or not), character, or symbol (including NIL).
;;;       start, end Delimit the subsring extracted from arg
           
(defun locale-string-downcase (arg &key (start 0) end)
  (declare (fixnum start))
  (let ((string (string arg)))
    (if #+lispworks (lw:base-string-p string)
        #+sbcl      (sb-kernel:base-string-p string)
        (string-upcase string :start start :end end)
        (ffc:%wstring-upper-down-case string start end nil))))

(defun locale-string-upcase (arg &key (start 0) end)
  (declare (fixnum start))
  (let ((string (string arg)))
    (if #+lispworks (lw:base-string-p string)
        #+sbcl      (sb-kernel:base-string-p string)
        (string-upcase string :start start :end end)
        (ffc:%wstring-upper-down-case string start end t))))

(defun locale-string-capitalize (arg &key (start 0) end)
  (declare (fixnum start))
  (let ((string (string arg)))
    (if #+lispworks (lw:base-string-p string)
        #+sbcl      (sb-kernel:base-string-p string)
        (string-capitalize string :start start :end end)
        (progn (setq string (ffc:%wstring-upper-down-case string start end nil))
          (when (plusp (length string))
            (setf (char string 0) (locale-char-upcase (char string 0))))
          string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FILE-TITLE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computing a file name to display in a window title bar basing on user preferences
;;; and avoiding the file extension or the path.
;;; GetFileTitle => 0 - succeess, <0 - file name is invalid, >0 - required buffer size
;;; Q: Is this LW 6.1 win32:short-namestring?

(defun get-file-title (pathname)
  (ffc:with-foreign ((buffer :wchar-t :nelems 255)
                     (lpszFile :ef-wc-string :initial-contents (namestring pathname)))
    (if (zerop (ffc::|GetFileTitle| lpszFile buffer 255))	; success
        (ffc:%cstring-lisp buffer :external-format #+lispworks :unicode
                                                   #+(and sbcl little-endian) :ucs-2le
                                                   #+(and sbcl big-endian) :ucs-2be)
        +null-string+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INITIALIZE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-locale ()
 ;;; Initialization taking place both at load-time and run-time in delivered image
  ;#+lispworks  (fli:set-locale)	; set C locale => "Russian_Russia.1251"
  #|(setq *internal-format*
        #+(and lispworks win32)       (if (string= (software-type) "Windows NT")
                                          :unicode		; UTF-16 = UCS-2
                                          win32:*multibyte-code-page-ef*)
        #+(and lispworks (not win32)) :latin-1		;STREAM::*DEFAULT-EXTERNAL-FORMAT*

        ;; SBCL Windows:
        ;;  We rely on 16-bit WCHAR_T , i.e. link to ..W rather than ..A  DLL entry points.
        #+(and sbcl win32 sb-unicode little-endian) :ucs-2le	; :x86 WCHAR_T
        #+(and sbcl win32 sb-unicode big-endian)    :ucs-2be	; => :cp1251
        #+(and sbcl (not (and win32 sb-unicode)))   sb-impl::*default-external-format*)
   |#

  #+win32
  (setq *decimal-separator* (schar (ffc:get-locale-info ffc::LOCALE_SDECIMAL) 0)

        *currency-thousand-separator* (let((s (ffc:get-locale-info
                                               ffc::LOCALE_SMONTHOUSANDSEP)))
                                        (when (plusp (length s)) (schar s 0)))
        *currency-digits* (parse-integer (ffc:get-locale-info ffc::LOCALE_ICURRDIGITS))
        *currency-symbol* (ffc:get-locale-info ffc::LOCALE_SCURRENCY)
        *currency-order* (parse-integer (ffc:get-locale-info ffc::LOCALE_ICURRENCY))

        *date-separator* (schar (ffc:get-locale-info ffc::LOCALE_SDATE) 0)
        *date-format*	 (or (cdr (assoc (schar (ffc:get-locale-info ffc::LOCALE_IDATE) 0)
                                         '((#\0 . :mdy) (#\1 . :dmy)))) :ymd)
        *century-digits* (* 2 (1+ (parse-integer (ffc:get-locale-info
                                                  ffc::LOCALE_ICENTURY))))

        *month-names*	 (apply #'vector (loop for info-type upfrom ffc::LOCALE_SMONTHNAME1
                                               repeat 12
                                               collect (ffc:get-locale-info info-type)))
        *month-abbrevs* (apply #'vector (loop for info-type
                                              upfrom ffc::LOCALE_SABBREVMONTHNAME1
                                              repeat 12
                                              collect (ffc:get-locale-info info-type)))
        *weekday-names*	 (apply #'vector (loop for info-type upfrom ffc::LOCALE_SDAYNAME1
                                               repeat 7
                                               collect (ffc:get-locale-info info-type)))
	*weekday-abbrevs*(apply #'vector (loop for info-type
                                               upfrom ffc::LOCALE_SABBREVDAYNAME1
                                               repeat 7
                                               collect (ffc:get-locale-info info-type)))
        *time-separator* (schar (ffc:get-locale-info ffc::LOCALE_STIME) 0)
        *time-format*	 (if (eq (schar (ffc:get-locale-info ffc::LOCALE_ITIME) 0) #\1)
                             24 12)
) )


#+(or lispworks sbcl)
(initialize-locale)

#+lispworks
(lw:define-action "When starting image" "Initialize locale" 'initialize-locale)
