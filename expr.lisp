;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:esrap)
  (:nicknames #:s/expr)
  (:export #:parse-expr))

(in-package #:streams/expr)

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defun msl-char-p (char)
  "Return true if char is a valid msl character."
  (or (alphanumericp char)
      (when (member char '(#\@ #\, #\- #\:))
        t)))

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

;;; (defrule alphanumeric (alphanumericp character))
(defrule alphanumeric (msl-char-p character))

(defrule string-char (or (not-doublequote character) (and #\\ #\")))

;;; An expr is either a list or an atom, with possibly leading whitespace.

(defrule expr (and (? whitespace) (or magic list atom))
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))

(defrule magic "b612"
  (:constant :magic)
  (:when (eq * :use-magic)))

(defrule list (and #\( expr (* expr) (? whitespace) #\))
  (:destructure (p1 car cdr w p2)
                (declare (ignore p1 p2 w))
                (cons car cdr)))

(defrule atom (or string integer symbol))

(defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
                (declare (ignore q1 q2))
                (text string)))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule symbol (not-integer (+ alphanumeric))
  (:lambda (list)
    (intern (text list))))

(defun parse-expr (expr)
  (parse 'expr expr))
