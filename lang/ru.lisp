;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Encoding: (win32:code-page :id 1251) -*-
;;; Ystok Library - Russian-related functions and translations of hard-wired strings
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

(defnative "Yes"	"ƒа")
(defnative "No"		"Ќет")
(defnative "Cancel"	"ќтменить")
(defnative "Error"	"ќшибка")

(defnative "Illegal number format ~S~@[, char '~C'~]."
 #+Russian "Ќедопустимый формат числа ~S~@[, литера '~C'~].")

(let (#+lispworks (dspec:*redefinition-action* :quiet))

(defun ordinalize-tail (quantity &optional (gender t))
 ;;; Change the case of ordinal numeral
  ;; ¬ыбрать окончание дл€ пор€дкового числительного quantity в именительном падеже ед.ч.
  ;; »змен€ть также по падежам и числам?
  (declare (ignore quantity))
  (case gender
    (:male	"-й")
    (:female	"-€")
    (otherwise	"-е")) )

(defun transliterate-char (char &aux i)
 ;;; Value: character, string or nil
  ;; Q: Should we use Unicode names or Adobe glyph names?
  (cond ((setq i (char-position char "јЅ¬√ƒ≈®«»… ЋћЌќѕ–—“”‘’џабвгдеЄзийклмнопрстуфхы"))
         (schar "ABVGDEEZIJKLMNOPRSTUFHYabvgdeezijklmnoprstufhy" i))
        ((setq i (char-position char "∆ж÷ц„чЎшўщЁэёюя€Џъ№ь"))
         (svref #("ZH" "zh" "TS" "ts" "CH" "ch" "SH" "sh" "SCH" "sch"
                  "EE" "ee" "JU" "ju" "JA" "ja"
                  nil nil nil nil)		                     ;"ereversed"
                  ;"Hardsign" "hardsign" "Softsign" "softsign"
                i))
        (char)))
)