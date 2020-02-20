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

;;
;; STREAM (Character) Parsers
;;

;; STREAM Tests (Don't return a value)

(defun ?whitespace ()
  "Match one or more whitespace characters."
  (?seq (%some (maxpc.char:?whitespace))))

;; STREAM Single-Value Getters

(defun =msl-key ()
  "Match and return a valid MSL key."
  (=subseq (?seq (%some (?satisfies 'alpha-char-p))
                 (%any (?seq (%maybe (?eq #\-)))
                       (%some (?satisfies 'alphanumericp))))))

;; Dummy Filespec
(defun =msl-filespec ()
 "Match a return a uri filespec or url."
 (=subseq (%some (?satisfies 'alphanumericp))))

;; Dummy SHA256
(defun =sha256 ()
  "Match and return a SHA256 value."
  (=subseq (%some (?satisfies 'alphanumericp))))

(defun =bracketed-transform ()
 "Match and return a bracketed tranform."
  (=destructure (_ url _)
    (=list (?eq #\[)
           (=msl-filespec)
           (?eq #\]))
    url))

(defun =msl-hash ()
 "Match and return a hash."
 (=destructure (_ hash)
  (=list (?eq #\#)
         (=sha256))
  hash))


(defun =msl-value ()
  "Match and return a raw value."
  (%some (?not (?seq (?eq #\right_parenthesis) (?end)))))

(defun =msl-comment ()
 "Match a comment."
  (=destructure (_ comment)
    (=list (maxpc.char:?string "//")
           (=msl-value)
      comment)))

;; STREAM List-of-Values Getters

(defun =msl-selector ()
 "Match and return a selector."
 (%or (=metadata-selector)))

(defun =metadata-selector ()
  "Match and return a metadata selector."
  (=destructure (_ key value)
    (=list (?eq #\:)
           (=msl-key)
           (%maybe (=destructure (_ value)
                     (=list (?whitespace)
                            (=msl-value)))))
    (list key value)))

(defun =msl-transform ()
 "Match a transform."
 (%or (=bracketed-transform)))

(defun =msl-comment ()
 "Match a comment."
 (=destructure (_ comment)
   (=list (maxpc.char:?string "//")
          (=msl-value))
   comment))

(defun =msl-prelude ()
    "Match and return a prelude."
    (=destructure (_ _ _ key value _)
        (=list (?eq #\left_parenthesis)
               (maxpc.char:?string "msl" nil)
               (?whitespace)
               (=msl-key)
               (=subseq (%maybe (?seq (?whitespace) (=msl-value))))
               ; (%some (=element))
               ; ; (%any (?seq (?whitespace) (%or (=msl-selector))
               ; ;                           (=msl-transform)
               ; ;                           (=msl-value)))
               ; ; (%maybe (=subseq (?seq (?whitespace) (=msl-hash))))
               ; ; (%maybe (=subseq (?seq (?whitespace) (=msl-comment))))
               (?eq #\right_parenthesis))
      (list key value)))
