;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
  (:nicknames #:s/expr)
  (:export #:parse-expr))

(in-package #:streams/expr)

(defun not-doublequote (char)
  "Return true if CHAR is not a double quote (U+0022)"
  (not (eql #\" char)))

(defun not-integer (string)
  "Return true if STRING is not an integer."
  (when (find-if-not #'digit-char-p string)
    t))

(defun extra-chars-p (char)
  "Return true if CHAR is one of non-standand characters."
  (when (member char '(#\@ #\, #\- #\:))
    t))

(defun ?alphanumeric ()
  "Return a parser that checks if an argument are letters, or extra characters."
  (%or (?satisfies 'alphanumericp)
       (?satisfies 'extra-chars-p)))

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
                                   (=subseq (%some (?alphanumeric)))))
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

(defun parse-expr (expr)
  "Parse expr according the rules listed above."
  (parse expr (=sexp)))
