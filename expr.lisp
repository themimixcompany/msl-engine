;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
  (:nicknames #:s/expr)
  (:export #:=sexp))

(in-package #:streams/expr)


;;;-----------------------------------------------------------------------------
;;; Old tokenizer
;;;-----------------------------------------------------------------------------

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
    (or (inp code #x21 #x22)            ; #\! #\"
        (inp code #x23 #x27)            ; #\# #\$ #\% #\& #\'
        (inp code #x2A #x2F)            ; #\* #\+ #\, #\- #\. #\/
        (inp code #x3A #x40)            ; #\: #\; #\< #\= #\> #\? #\@
        (inp code #x5B #x60)            ; #\[ #\\ #\] #\^ #\_ #\`
        (inp code #x7B #x7E)            ; #\{ #\| #\} #\~
        (>= code #x7F))))               ; other characters

(defun ?msl-char-p ()
  "Return a parser that checks if an argument are letters, numbers, or extra characters."
  (%or (?satisfies 'alphanumericp)
       (?satisfies 'extra-char-p)))

(defun =symbol ()
  "Return a parser that checks if an argument is a symbol."
  (=transform (=subseq (?satisfies 'not-integer (=subseq (%some (?msl-char-p)))))
              'intern))

(defun =atom ()
  "Return a parser that checks if an argument is an atom."
  (%or (maxpc.digit:=integer-number)
       (=symbol)))

(defun =sexp ()
  "Return a parser for handling s-expressions."
  (%or '=slist/parser (=atom)))

(defun =slist ()
  "Return a parser for handling s-expressions in parens."
  (=destructure (_ expressions _ _)
      (=list (?eq #\()
             (%any (=destructure (_ expression)
                       (=list (%any (maxpc.char:?whitespace))
                              '=sexp/parser)))
             (%any (maxpc.char:?whitespace))
             (?eq #\)))))

(setf (fdefinition '=sexp/parser) (=sexp)
      (fdefinition '=slist/parser) (=slist))


;;;-----------------------------------------------------------------------------
;;; New tokenizer
;;;-----------------------------------------------------------------------------

(defun =whitespace ()
  "Return a parser that matches whitespaces."
  (?seq (%some (maxpc.char:?whitespace))))

(defun namespacep (ns)
  "Return true if NS is a namespace character."
  (member ns '(#\m #\w #\s #\v #\c #\@ #\f #\d)))

(defun =namespace ()
  "Return a parser that matches a namespace character."
  (=subseq (?satisfies 'namespacep)))

(defun =key ()
  "Return a parser that matches a key."
  (=subseq (?seq (%some (?satisfies 'alphanumericp))
                 (%any (?seq (%maybe (?eq #\-))
                             (%some (?satisfies 'alphanumericp)))))))

(defun =value ()
  "Return a parser that matches a value."
  (=subseq (%any (?not (?eq #\))))))

(defun =ns-and-key ()
  "Return a parser that matches a namespace and key."
  (%or (=destructure (namespace _ key)
           (=list (=namespace)
                  (=whitespace)
                  (=key))
         (list namespace key))
       (=destructure (namespace key)
           (=list (=subseq (?eq #\@))
                  (=key))
         (list namespace key))))

(defun =uri ()
  "Return a parser for handling URIs."
  (=subseq (%some (?satisfies 'alphanumericp))))

(defun =bracketed-transform ()
  "Return a parser for handling bracketed [] transforms."
  (=destructure (_ value _)
      (=list (?eq #\[)
             (=uri)
             (?eq #\]))
    value))

(defun =msl-expr ()
  "Return a parser for handling msl expressions."
  (=destructure (_ ns-and-key _ value _)
      (=list (?eq #\()
             (=ns-and-key)
             (=whitespace)
             (%or '=msl-expr/parser (=bracketed-transform) (=value))
             (?eq #\)))
    (append ns-and-key (list value))))

;;; This hack is necessary to allow recursive parsing. When updating =MSL-EXPR,
;;; this expression has to be re-evaluated, too.
(setf (fdefinition '=msl-expr/parser) (=msl-expr))
