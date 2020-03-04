;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
  (:nicknames #:s/expr))

(in-package #:streams/expr)



;;
;; Utility (Non-Parser) Functions
;;

(defun hex-char-p (char)
  "Return true if CHAR is valid hexadecimal character."
  (or (digit-char-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))
;;

(defun length-64-p (value)
  "Return true if VALUE is 64 characters long."
  (= (length value) 64))
;;

;;
;; STREAM (Character) Parsers
;;

;; STREAM Tests (Don't return a value)

(defun ?whitespace ()
  "Match one or more whitespace characters."
  (?seq (%some (maxpc.char:?whitespace))))

(defun ?hexp ()
  "Match a single hex character."
  (?satisfies 'hex-char-p))

;; STREAM Single-Value Getters

(defun =sha256 ()
  "Match and return a SHA-256 string."
  (=subseq (?seq (?satisfies 'length-64-p
                   (=subseq (%some (?hexp)))))))
;;

(defun =msl-key ()
  "Match and return a valid MSL key."
  (=subseq (?seq (%some (?satisfies 'alpha-char-p))
                 (%any (?seq (%maybe (?eq #\-))
                         (%some (?satisfies 'alphanumericp)))))))

;; Dummy Filespec
(defun =msl-filespec ()
 "Match and return a URI filespec or URL."
 (=subseq (%some (?satisfies 'alphanumericp))))

(defun =bracketed-transform ()
 "Match and return a bracketed tranform."
  (=destructure (_ url _)
    (=list (?eq #\[)
           (=msl-filespec)
           (?eq #\]))
    url))
;;

(defun =msl-hash ()
  "Match and return a hash value."
  (=destructure (_ _ hash)
      (=list (?whitespace)
             (?eq #\#)
             (=sha256))))
;;

(defun =msl-value ()
  "Match and return a raw value."
  (%and
    (?not (%or (=metadata-getter)
               'regex-getter/parser
               (=msl-hash)
               (=msl-comment)))
    (=destructure (_ value)
      (=list
        (?whitespace)
        (=subseq (%some (?not (%or (=metadata-getter)
                                   'regex-getter/parser
                                   (=msl-hash)
                                   (=msl-comment)
                                   (?seq (?eq #\right_parenthesis) (?end))))))))))
;;

(defun =msl-comment ()
 "Match a comment."
  (=destructure (_ _ comment)
    (=list (?whitespace)
           (maxpc.char:?string "//")
           (=subseq (%some (?not (?seq (?eq #\right_parenthesis) (?end))))))))
;;

;; STREAM List-of-Values Getters

(defun =msl-selector ()
 "Match and return a selector."
 (%or (=metadata-selector)))
;;

(defun =metadata-selector ()
  "Match and return a metadata selector."
  (=destructure (_ key value)
    (=list (?eq #\:)
           (=msl-key)
           (%maybe (=destructure (_ value)
                     (=list (?whitespace)
                            '=msl-value/parser))))
    (list key value)))
;;

(defun =msl-transform ()
 "Match a transform."
 (%or (=bracketed-transform)))
;;


(defun =atom-namespace ()
  "Match and return an atom namespace."
  (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v #\c #\@))))))
;;

(defun =grouping-namespace ()
    "Match and return m w s v namespace."
    (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v))))))
;;

(defun =@-namespace ()
    "Match and return the @ namespace."
    (=subseq (?eq #\@)))
;;



(defun =single-getter ()
  "Return the components for a single mx-read operation."
  (%or (=atom-getter)))
;;

(defun =metadata-selector ()
  "Match and return the key sequence for a : selector."
  ())
;;

(defun =grouping-getter ()
  "Match and return the key sequence for an atom."
  (=list (=destructure (ns _)
           (=list (=grouping-namespace)
                  (?whitespace)))
         (=msl-key)))
;;

(defun =canon-getter ()
  "Match and return the key sequence for canon."
  (=list (=destructure (ns _)
           (=list (=canon-namespace)
                  (?whitespace)))
         (=msl-key)))
;;

(defun =@-getter ()
  "Match and return the key sequence for an @."
  (=list (=destructure (ns _)
           (=list (=@-namespace)
                  (%maybe (?whitespace))))
         (=msl-key)))
;;


(defun =regex-getter ()
  "Match and return the key sequence for /."
  (=destructure (_ _ regex _ env value)
    (=list (?whitespace)
           (=regex-namespace)
           (=subseq (%some (?satisfies 'alphanumericp)))
           (=regex-namespace)
           (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
           (%maybe (=msl-value)))
    (list regex env value)))
;;

(defun =metadata-getter ()
 "Match and return key sequence for :."
  (=destructure (_ ns key)
    (=list (?whitespace)
           (=metadata-namespace)
           (=msl-key))
    (list ns key)))
;;

(defun =metadata-namespace ()
  "Match and return the : namespace."
  (=subseq (?eq #\:)))
;;

(defun =canon-namespace ()
    "Match and return the c namespace."
    (=subseq (?eq #\c)))
;;

(defun =datatype-namespace ()
        "Match and return the d namespace."
        (=subseq (?eq #\d)))
;;

(defun =datatype-getter ()
  "Match and return key sequence for d."
  (=destructure (atom _ key)
    (=list (=datatype-namespace)
           (?whitespace)
           (=msl-key))
    (list atom key)))
;;

(defun =regex-namespace ()
    "Match and return the / namespace."
    (=subseq (?eq #\/)))
;;

(defun =grouping-form ()
  "Match and return an atom in m w s v namespace."
  (=destructure (_ getter-setter _ _)
    (=list (?eq #\left_parenthesis)
           (%or (=destructure (atom _ sub)
                  (=list (=grouping-getter)
                         (?whitespace)
                         (=subatomic-getter))
                  (list atom sub))
                (=single-getter))
           (?eq #\right_parenthesis)
           (?end))))
;;

(defun =canon-form ()
  "Match and return an atom in c namespace."
  (=destructure (_ getter-setter _ _)
    (=list (?eq #\left_parenthesis)
           (=destructure (atom _ sub)
                  (=list (=canon-getter)
                         (?whitespace)
                         (=metadata-getter))
                  (list atom sub))
           (?eq #\right_parenthesis)
           (?end))))
;;

(defun =datatype-form ()
  "Match and return an atom in d namespace."
  (=destructure (_ getter-setter _ _)
    (=list (?eq #\left_parenthesis)
           (%or (=destructure (atom _ sub)
                  (=list (=datatype-getter)
                         (?whitespace)
                         (=metadata-getter))
                  (list atom sub))
                (=datatype-getter))
           (?eq #\right_parenthesis)
           (?end))))
;;


(defun =@-form ()
   "Match and return an atom in the @ namespace."
   (=destructure (_ atom-seq atom-value atom-regex sub-list hash comment _ _)
                 (=list (?eq #\left_parenthesis)
                        (=@-getter)
                        (%maybe (=msl-value))
                        (%maybe (%some (=regex-getter)))
                        (%maybe (%or
                                  (%some (=list (=metadata-getter)
                                                (=msl-value)
                                                (%maybe (%some (=regex-getter)))))
                                  (=list (=metadata-getter)
                                         (%maybe (=msl-value))
                                         (%maybe (%some (=regex-getter))))))
                        (%maybe (=msl-hash))
                        (%maybe (=msl-comment))
                        (?eq #\right_parenthesis)
                        (?end))
                 (list atom-seq atom-value (list "/" atom-regex) sub-list hash comment)))
;;

;; DESIRED OUTPUT:
;; (("@" "WALT") "Walt Disney" (((":" "birthday") "1901") ((":" "wife") "Lillian") "50d858e0985ecc7f60418aaf0cc5ab587f42c2570a884095a9e8ccacd0f6545c" "comment"


  ;;
  ;; Function-namespace definitions for recursive functions
  ;;

(setf (fdefinition '=msl-value/parser) (=msl-value)
      (fdefinition '=@-form/parser) (=@-form)
      (fdefinition 'regex-getter/parser) (=regex-getter))
