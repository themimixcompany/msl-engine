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

(defun =satom ()
  "Return a parser that checks if an argument is an atom."
  (%or (maxpc.digit:=integer-number)
       (=symbol)))

(defun =sexp ()
  "Return a parser for handling s-expressions."
  (%or '=slist/parser (=satom)))

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
  "Return a parser that matches one or more whitespaces."
  (?seq (%some (maxpc.char:?whitespace))))

(defun namespacep (ns)
  "Return true if NS is a namespace character."
  (member ns '(#\m #\w #\s #\v #\c #\@ #\f #\d)))

(defun =namespace ()
  "Return a parser that matches a namespace character."
  (=subseq (?satisfies 'namespacep)))

(defun =msl-key ()
  "Return a parser that matches a key."
  (=subseq (?seq (%some (?satisfies 'alphanumericp))
                 (%any (?seq (%maybe (?eq #\-))
                             (%some (?satisfies 'alphanumericp)))))))

(defun =msl-value ()
  "Return a parser that matches a value."
  (=subseq (%any (?not (?eq #\))))))

(defun =ns-and-key ()
  "Return a parser that matches a namespace and key."
  (%or (=destructure (namespace _ key)
           (=list (=namespace)
                  (=whitespace)
                  (=msl-key))
         (list namespace key))
       (=destructure (namespace key)
           (=list (=subseq (?eq #\@))
                  (=msl-key))
         (list namespace key))))

(defun =msl-expr ()
  "Return a parser for handling msl expressions."
  (=destructure (_ ns-and-key _ value _)
      (=list (?eq #\()
             (=ns-and-key)
             (=whitespace)
             (%or '=msl-expr/parser (=bracketed-transform) (=msl-value))
             (?eq #\)))
    (append ns-and-key (list value))))

;;; This hack is necessary to allow recursive parsing. When updating =MSL-EXPR,
;;; this expression has to be re-evaluated, too.
(setf (fdefinition '=msl-expr/parser) (=msl-expr))


;;;-----------------------------------------------------------------------------
;;; New stuff
;;;-----------------------------------------------------------------------------

(defun =whitespace* ()
  "Return a parser that matches zero or more whitespaces."
  (?seq (%any (maxpc.char:?whitespace))))

(defun hex-letter-p (char)
  "Return true if CHAR is a valid hexadecimal letter (A-F)."
  (let ((c (char-code char)))
    (or (inp c #x41 #x46)
        (inp c #x61 #x66))))

(defun ?hexp ()
  "Return a parser to test if input is a hexidecimal character."
  (%or (?satisfies 'numberp)
       ;; (?satisfies 'hex-letter-p)
       (?satisfies 'alphanumericp)))

(defun =sha256 ()
  "Return true if STRING looks like a SHA-256 string."
  (=subseq (%some (?hexp))))

(defun =msl-hash ()
  "Return a parser for hashes."
  (=destructure (_ hash)
      (=list (?eq #\#)
             (=sha256))
    (when (= (length hash) 64)
      hash)))

(defvar *whitespace*
  '(#\Space #\Tab #\Vt #\Newline #\Page #\Return #\Linefeed)
  "A list of characters considered as whitespace.")

(defun whitespacep (char)
  "Return true if CHAR is a whitespace character."
  (when (member char *whitespace*)
    t))

(defun msl-char-p (char)
  "Return true if CHAR can be used as value data."
  (or (alphanumericp char)
      (whitespacep char)))

(defun =msl-comment ()
  "Return a parser for comments."
  (=destructure (_ _ comment)
      (=list (?seq (?eq #\/) (?eq #\/))
             (=whitespace*)
             (=subseq (%some (?satisfies 'msl-char-p))))
    comment))

(defun =msl-uri ()
  "Return a parser for handling URIs."
  (=subseq (%some (?satisfies 'alphanumericp))))

(defun =msl-filespec ()
  "Return a parser for handling filespecs."
  (%or (=destructure (_ value)
           (=list (?seq (?eq #\f) (?eq #\i) (?eq #\l) (?eq #\e)
                        (?eq #\:) (?eq #\/) (?eq #\/))
                  (=subseq (%some (?satisfies 'alphanumericp))))
         value)))

(defun =msl-bracketed-transform ()
  "Return a parser for handling bracketed [] transforms."
  (=destructure (_ value _)
      (=list (?eq #\[)
             (%or (=msl-uri) (=msl-filespec))
             (?eq #\]))
    value))

(defun =msl-format ()
  "Return a parser for format expressions."
  (=destructure (_ _ _ key _ value _ comment _ _)
      (=list (?eq #\()
             (?eq #\f)
             (=whitespace)
             (=msl-key)
             (=whitespace)
             (%some (%or (=msl-format) (=msl-selector) (=msl-value)))
             (%any (=msl-comment))
             (=whitespace*)
             (?eq #\)))
    (list key value)))

(defun =msl-transform ()
  "Return a parser for transforms."
  (%or (=msl-bracketed-transform)
       (=msl-format)))

(defun =msl-expression ()
  "Return a parser for handling expressions."
  (%some (%or (=msl-prelude)
              (=msl-machine)
              (=msl-world)
              (=msl-stream)
              (=msl-view)
              (=msl-atom)
              (=msl-selector)
              (=msl-value)
              (=msl-transform)
              (=msl-comment))))

(defun =msl-prelude ()
  "Return a parser for prelude."
  (=destructure (_ _ _ _ key _ value _ hash _ comment _ _)
      (=list (?eq #\()
             (=whitespace*)
             (?seq (?eq #\m) (?eq #\s) (?eq #\l))
             (=whitespace)
             (=msl-key)
             (=whitespace)
             (=msl-value)
             (=whitespace)
             (%any (=msl-hash))
             (=whitespace)
             (%any (=msl-comment))
             (=whitespace*)
             (?eq #\)))
    (list key value)))

;;; Refactor between =MSL-PRELUDE and =MSL-MACHINE
(defun =msl-machine ()
  "Return a parser for machines."
  (=destructure (_ _ _ _ key _ value _ hash _ comment _ _)
      (=list (?eq #\()
             (=whitespace*)
             (?eq #\m)
             (=whitespace*)
             (=msl-key)
             (=whitespace*)
             (=msl-value)
             (=whitespace*)
             (=msl-hash)
             (=whitespace*)
             (=msl-comment)
             (=whitespace*)
             (?eq #\)))))

(defun =msl-world ()
  "Return a parser for worlds."
  nil)

(defun =msl-stream ()
  "Return a parser for streams."
  nil)

(defun =msl-view ()
  "Return a parser for views."
  nil)

(defun =msl-canon ()
  "Return a parser for canons."
  nil)

(defun =msl-atom ()
  "Return a parser for atoms."
  nil)

(defun =msl-metadata-selector ()
  "Return a parser for metadata selectors."
  nil)

(defun =msl-regex-selector ()
  "Return a parse for regexes."
  nil)

(defun =msl-selector ()
  "Return a parser for selectors."
  (%or (=msl-metadata-selector)
       (=msl-regex-selector)))


;;; Misc

(defun =xatom ()
  (%or (maxpc.digit:=integer-number)
       (=symbol)))

(defun =xexpr ()
  (=destructure (_ e _)
      (=list (?eq #\()
             (%any (=destructure (_ e)
                       (=list (%or (=xatom)
                                   '=xexpr/parser))))
             (?eq #\)))))

(setf (fdefinition '=xexpr/parser) (=xexpr))
