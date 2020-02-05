;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use
     #:cl #:maxpc)
  (:nicknames #:s/expr)
  (:export #:=sexp
           #:parse-expr))

(in-package #:streams/expr)

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defun extra-chars-p (char)
  "Return true if CHAR is one of non-standand characters."
  (when (member char '(#\@ #\, #\- #\:))
    t))

(defun ?alphanumeric ()
  (%or (?satisfies 'alphanumericp)
       (?satisfies 'extra-chars-p)))

(defun ?string-char ()
  (%or (?seq (?eq #\\) (?eq #\"))
       (?satisfies 'not-doublequote)))

(defun =atom ()
  (%or (=string) (maxpc.digit:=integer-number) (=symbol)))

(defun =string ()
  (=destructure (_ s _)
      (=list (?eq #\")
             (=subseq (%any (?string-char)))
             (?eq #\"))))

(defun =symbol ()
  (=transform (=subseq (?satisfies 'not-integer
                                   (=subseq (%some (?alphanumeric)))))
              'intern))

(defun =sexp ()
  (%or '=slist/parser (=atom)))

(defun =slist ()
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
