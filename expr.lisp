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
    (?not (?value-terminator))
    (=destructure (_ value)
      (=list
        (?whitespace)
        (=subseq (%some (?not (?value-terminator))))))))
;;

(defun ?value-terminator ()
  "Match the end of a value."
  (%or (=metadata-getter)
       'regex-getter/parser
       'bracketed-transform-getter/parser
       'datatype-form/parser
       (=msl-hash)
       (=msl-comment)
       (?seq (?eq #\right_parenthesis) (=metadata-getter))
       (?seq (?eq #\right_parenthesis) 'datatype-form/parser)
       (?seq (?eq #\right_parenthesis) (?eq #\right_parenthesis))
       (?seq (?eq #\right_parenthesis) (?end))))
;;

(defun ?expression-terminator ()
  "Match the end of an expression."
    (?seq
      (?eq #\right_parenthesis)))

(defun =msl-comment ()
 "Match a comment."
  (=destructure (_ _ comment)
    (=list (?whitespace)
           (maxpc.char:?string "//")
           (=subseq (%some (?not (?seq (?eq #\right_parenthesis) (%or (?end)
                                                                      'msl-comment/parser))))))))
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
  (=destructure (regex-list)
                (=list
                  (%some
                    (=destructure (_ _ regex _ env value)
                      (=list (?whitespace)
                             (=regex-namespace)
                             (=subseq (%some (?satisfies 'alphanumericp)))
                             (=regex-namespace)
                             (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                             (%maybe (=msl-value)))
                      (list regex env value))))
                (cond (regex-list (list "/" regex-list)))))

;;

(defun =bracketed-transform-getter ()
 "Match and return a bracketed transform."
  (=destructure (transform-list)
    (=list
      (%some
        (=destructure (_ _ url _)
          (=list (?whitespace)
                 (?eq #\[)
                 (=msl-filespec)
                 (?eq #\])))))

    (cond (transform-list (list "[]" transform-list)))))
;;

(defun =subatomic-getter ()
  "Match and return key sequence for / [] d namespace."
  (%or (=regex-getter)
       (=datatype-form)
       (=bracketed-transform-getter)))

(defun =metadata-getter ()
 "Match and return key sequence for : namespace."
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




(defun =@-form ()
   "Match and return an atom in the @ namespace."
   (=destructure (_ atom-seq atom-value atom-sub-list metadata-list hash comment _ _)
                 (=list (?eq #\left_parenthesis)
                        (=@-getter)
                        (%maybe (=msl-value))
                        (%any (=subatomic-getter))
                        (%maybe (%or
                                    (%some (=destructure (meta-keys meta-value meta-sub-list)
                                            (%or
                                              (=list (=metadata-getter)
                                                     (=msl-value)
                                                     (%any (=subatomic-getter)))
                                              (=list (=metadata-getter)
                                                     (%maybe (=msl-value))
                                                     (%some (=subatomic-getter))))
                                            (list meta-keys meta-value meta-sub-list)))
                                    (=destructure (meta-keys meta-value meta-sub-list)
                                      (=list (=metadata-getter)
                                             (%maybe (=msl-value))
                                             (%any (=subatomic-getter)))
                                      (list meta-keys meta-value meta-sub-list))))
                        (%maybe (=msl-hash))
                        (%maybe (=msl-comment))
                        (?eq #\right_parenthesis)
                        (?end))
                 (list atom-seq atom-value atom-sub-list metadata-list hash comment)))
;;

(defun =datatype-form ()
   "Match and return an atom in the d namespace."
   (=destructure (_ _ atom-seq atom-value atom-regex sub-list comment _)
                 (=list (?whitespace)
                        (?eq #\left_parenthesis)
                        (=datatype-getter)
                        (%maybe (=msl-value))
                        (%maybe (=regex-getter))
                        (%maybe (%or
                                    (%some (=destructure (meta-keys meta-value sub-list)
                                            (%or
                                              (=list (=metadata-getter)
                                                     (=msl-value)
                                                     (%any (=regex-getter)))
                                              (=list (=metadata-getter)
                                                     (%maybe (=msl-value))
                                                     (%some (=regex-getter))))
                                            (list meta-keys meta-value sub-list)))
                                    (=destructure (meta-keys meta-value sub-list)
                                      (=list (=metadata-getter)
                                             (%maybe (=msl-value))
                                             (%any (=regex-getter)))
                                      (list meta-keys meta-value sub-list))))
                        (%maybe (=msl-comment))
                        (?expression-terminator))
                 (list atom-seq atom-value atom-regex NIL sub-list NIL comment)))
;;
;;

;; STANDARD OUTPUT:

;; (parse "(@WALT Walt Disney /wregex1/wenv1 wconsume1 wconsume2 /wregex2/wenv2 [wt1] [wt2] :wife Lillian /lregex/ :birthday [btransform])" (=@-form))
;;
;; (("@" "WALT") "Walt Disney" (("/" (("wregex1" "wenv1" "wconsume1 wconsume2") ("wregex2" "wenv2" NIL))) ("[]" ("wt1" "wt2"))) (((":" "wife") "Lillian" (("/" (("lregex" NIL NIL))))) ((":" "birthday") NIL (("[]" ("btransform"))))) NIL NIL)

;; EMBEDDING AN ATOM
;; (parse "(@bio Walt Disney was born in (@WALT :birthplace).)" (=@-form))
;; (("@" "bio") "Walt Disney was born in (("@" "WALT") NIL NIL ((":" "birthplace") NIL NIL) NIL NIL)." NIL NIL NIL NIL)


  ;;
  ;; Function-namespace definitions for recursive functions
  ;;

(setf (fdefinition '=msl-value/parser) (=msl-value)
      (fdefinition '=@-form/parser) (=@-form)
      (fdefinition 'regex-getter/parser) (=regex-getter)
      (fdefinition 'bracketed-transform-getter/parser) (=bracketed-transform-getter)
      (fdefinition 'datatype-form/parser) (=datatype-form)
      (fdefinition 'msl-comment/parser) (=msl-comment))
