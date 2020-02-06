;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
  (:nicknames #:s/expr)
  (:export #:=sexp))

(in-package #:streams/expr)

(defun not-doublequote (char)
  "Return true if CHAR is not a double quote (U+0022)"
  (not (eql #\" char)))

(defun not-integer (string)
  "Return true if STRING is not an integer."
  (when (find-if-not #'digit-char-p string)
    t))

(defun inp (integer start &optional end)
  "Return true if INTEGER is within START and END, inclusively."
  (if end
      (and (>= integer start)
           (<= integer end))
      (= integer start)))

(defun extra-char-p (char)
  "Return true if CHAR is one of supplementary characters."
  (let ((code (char-code char)))
    (or (inp code #x21)
        (inp code #x23 #x27)
        (inp code #x2A #x2F)
        (inp code #x3A #x40)
        (inp code #x5B #x60)
        (inp code #x7B #x7E)
        (>= code #x7F))))

(defun ?msl-char-p ()
  "Return a parser that checks if an argument are letters, numbers, or extra characters."
  (%or (?satisfies 'alphanumericp)
       (?satisfies 'extra-char-p)))

(defun ?string-char ()
  "Return a parser that checks if an argument is the string marker."
  (%or (?seq (?eq #\\) (?eq #\"))
       (?satisfies 'not-doublequote)))

(defun =atom ()
  "Return a parser that checks if an argument is an atom."
  (%or (=string) (maxpc.digit:=integer-number) (=symbol)))

(defun =string ()
  "Return a parser that checks if an argument is a srting."
  (=destructure (_ s _)
      (=list (?eq #\")
             (=subseq (%any (?string-char)))
             (?eq #\"))))

(defun =symbol ()
  "Return a parser that checks if an argument is a symbol."
  (=transform (=subseq (?satisfies 'not-integer
                                   (=subseq (%some (?msl-char-p)))))
              'intern))

(defun =sexp ()
  "Return a parser for handling s-expressions."
  (%or '=slist/parser (=atom)))

(defun =slist ()
  "Return a parser for handling s-expressions in parens."
  (=destructure (_ expressions _ _)
      (=list (?eq #\()
             (%any (=destructure (_ expression)
                       (=list (%any (maxpc.char:?whitespace)) '=sexp/parser)))
             (%any (maxpc.char:?whitespace))
             (?eq #\)))))

(setf (fdefinition '=sexp/parser) (=sexp)
      (fdefinition '=slist/parser) (=slist))
