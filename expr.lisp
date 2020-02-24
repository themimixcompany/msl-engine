;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
  (:nicknames #:s/expr)
  (:export #:=sexp
           #:=xexpr))

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

(defun ?whitespace ()
  "Match one or more whitespace characters."
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

(defun =msl-selector ()
  "Return a parser for selectors."
  ;; (%or (=msl-metadata-selector)
  ;;      (=msl-regex-selector))
  (=subseq (?seq (?eq #\:) (=msl-key))))

(defun =msl-value ()
  "Return a parser that matches a value."
  ;(=subseq (%any (?not (=msl-selector))))
  (=subseq (%some (%and (?not (=msl-selector))
                        (?not (?eq #\)))))))

(defun =ns-and-key ()
  "Return a parser that matches a namespace and key."
  (%or (=destructure (namespace _ key)
           (=list (=namespace)
                  (?whitespace)
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
             (?whitespace)
             (%or '=msl-expr/parser (=msl-value))
             (?eq #\)))
    (append ns-and-key (list value))))

;; This hack is necessary to allow recursive parsing. When updating =MSL-EXPR,
;; this expression has to be re-evaluated, too.
(setf (fdefinition '=msl-expr/parser) (=msl-expr))


;;;-----------------------------------------------------------------------------
;;; New parsers
;;;-----------------------------------------------------------------------------

(defun ?whitespace* ()
  "Return a parser that matches zero or more whitespaces."
  (?seq (%any (maxpc.char:?whitespace))))

(defun hex-char-p (char)
  "Return true if CHAR is valid hexadecimal character."
  (or (digit-char-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(defun ?hexp ()
  "Return a parser to test if input is a hexidecimal character."
  (?satisfies 'hex-char-p))

(defun length-64-p (value)
  "Return true if VALUE is 64 characters long."
  (= (length value) 64))

(defun =sha256 ()
  "Return a parser to extract a SHA-256 string."
  (=subseq (?seq (?satisfies 'length-64-p (=subseq (%some (?hexp))))
                 (?end))))

(defun =msl-hash ()
  "Return a parser for hashes."
  (=destructure (_ v)
      (=list (?eq #\#)
             (=sha256))))

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
      (whitespacep char)
      (extra-char-p char)))

(defun =msl-comment ()
  "Return a parser for comments."
  (=destructure (_ _ comment)
      (=list (maxpc.char:?string "//" nil)
             (?whitespace*)
             (=subseq (%some (?satisfies 'msl-char-p))))
    comment))

(defun =msl-uri ()
  "Return a parser for handling URIs."
  (=subseq (?seq (%or (maxpc.char:?string "https" nil)
                      (maxpc.char:?string "http" nil)
                      (maxpc.char:?string "ftp" nil)
                      (maxpc.char:?string "file" nil)
                      (maxpc.char:?string "data" nil))
                 (maxpc.char:?string "://" nil)
                 (%some (?not (maxpc.char:?whitespace))))))

(defun =msl-filespec ()
  "Return a parser for handling filespecs."
  (%or (=destructure (_ value)
           (=list (maxpc.char:?string "file://" nil)
                  (=subseq (%some (?satisfies 'alphanumericp))))
         value)))

(defun =msl-bracketed-transform ()
  "Return a parser for handling bracketed [] transforms."
  (=destructure (_ value _)
      (=list (?eq #\[)
             (%or (=msl-uri) (=msl-filespec))
             (?eq #\]))
    value))

(defun =msl-transform ()
  "Return a parser for transforms."
  (%or (=msl-bracketed-transform)
       ;; (=msl-format)
       ))

(defun =atom-namespace ()
  "Return a parser that matches a namespace token."
  (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v #\c #\@)))))

(defun =atom-form ()
  "Return a parser for handling mx-atom forms."
  (=destructure (_ namespace _ key value _)
      (=list (?eq #\()
             (=atom-namespace)
             (?whitespace)
             (=msl-key)
             (%any (?seq (?whitespace)
                         (%or (=msl-selector)
                              (=msl-transform)
                              '=atom-form/parser
                              (=msl-value))))
             (?eq #\)))
    (list namespace key value)))

(setf (fdefinition '=atom-form/parser) (=atom-form))

(defun =msl-format ()
  "Return a parser for format expressions."
  (=destructure (_ _ _ key value _)
      (=list (?eq #\()
             (?eq #\f)
             (?whitespace)
             (=msl-key)
             (%or (%some (=subseq (?seq (?whitespace)
                                        (=msl-selector)
                                        (%maybe (=msl-value)))))
                  (=subseq (?seq (?whitespace)
                                 (=msl-value))))
             ;; (?whitespace)
             ;; (=msl-value)
             (?eq #\)))
    (list key value)))

(setf (fdefinition '=msl-format/parser) (=msl-format))

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
  (=destructure (_ _ _ key _ value _ hash _)
      (=list (?eq #\()
             (maxpc.char:?string "msl" nil)
             (?whitespace)
             (=msl-key)
             (?whitespace)
             (=msl-value)
             (?whitespace)
             (%maybe (=msl-hash))
             ;; (?whitespace)
             ;; (%maybe (=msl-comment))
             (?eq #\)))
    (list key value hash)))

(defun =msl-machine ()
  "Return a parser for machines."
  nil)

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
