;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
  (:nicknames #:s/expr)
  (:export #:=sexp))

(in-package #:streams/expr)


;;;-----------------------------------------------------------------------------
;;; New tokenizer
;;;-----------------------------------------------------------------------------

(defun ?whitespace ()
  "Match one or more whitespace characters, returning nothing."
  (?seq (%some (maxpc.char:?whitespace))))

  ; (defun =msl-prelude ()
  ;   "Return a parser for prelude."
  ;   (=destructure (_ _ _ key value hash comment)
  ;       (=list (?eq #\()
  ;              (maxpc.char:?string "msl" nil)
  ;              (?whitespace)
  ;              (=msl-key)
  ;              (%any (?seq (?whitespace) (%or (=msl-selector)
  ;                                        (=msl-transform)
  ;                                        (=msl-value))))
  ;              (%maybe (?seq (?whitespace) (=msl-hash)))
  ;              (%maybe (?seq (?whitespace) (=msl-comment)))
  ;              (?eq #\)))
  ;     (list key value hash comment)))
