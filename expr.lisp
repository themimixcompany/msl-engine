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
  (=destructure (_ v)
      (=list (?eq #\#)
             (=sha256))))
;;


(defun =msl-value ()
  "Match and return a raw value."
  (=subseq (%some (?not (%or (?seq (?whitespace) (=msl-selector))
                             (?seq (?whitespace) (=msl-hash))
                             (?seq (?whitespace) (=msl-comment))
                             (?seq (?eq #\right_parenthesis) (?end)))))))
;;

(defun =msl-comment ()
 "Match a comment."
  (=destructure (_ comment)
    (=list (maxpc.char:?string "//")
           (=subseq (%some (?not (?seq (?eq #\right_parenthesis) (?end))))))
    comment))

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
                            '=msl-value/parser))))
    (list key value)))

(defun =msl-transform ()
 "Match a transform."
 (%or (=bracketed-transform)))

(defun =msl-prelude ()
    "Match and return a prelude."
    (=destructure (_ _ _ key value _)
        (=list (?eq #\left_parenthesis)
               (maxpc.char:?string "msl" nil)
               (?whitespace)
               (=msl-key)
               (=subseq (%maybe (?seq (?whitespace) (=msl-value))))
               ; ; (%any (?seq (?whitespace) (%or (=msl-selector))
               ; ;                           (=msl-transform)
               ; ;                           (=msl-value)))
               ; ; (%maybe (=subseq (?seq (?whitespace) (=msl-hash))))
               ; ; (%maybe (=subseq (?seq (?whitespace) (=msl-comment))))
               (?eq #\right_parenthesis))
      (list key value)))
;;

(defun =atom-namespace ()
  "Match and return an atom namespace."
  (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v #\c #\@))))))

(defun =@-namespace ()
    "Match and return the @ namespace."
    (=subseq (?eq #\@)))



(defun =msl-atom ()
  "Match and return an atom."
  (=destructure (_ ns-and-key value selector hash comment _)
    (=list (?eq #\left_parenthesis)
           (%or (=list (=@-namespace)
                       (=msl-key))
                (=destructure (ns _ key)
                  (=list (=atom-namespace)
                         (?whitespace)
                         (=msl-key))
                  (list ns key)))
           (%maybe (=destructure (_ value)
                     (=list (?whitespace)
                            (%or (=msl-value)))))
           (%any (=destructure (_ value)
                     (=list (?whitespace)
                            (=msl-selector))))
           (%maybe (=destructure (_ hash)
                     (=list (?whitespace)
                            (=msl-hash))))
           (%maybe (=destructure (_ comment)
                     (=list (?whitespace)
                            (=msl-comment))))
           (?eq #\right_parenthesis))
    (list ns-and-key value selector hash comment)))

(defun =single-getter ()
  "Return the components for a single mx-read operation."
  (%or (=atom-getter)))

;

(defun =metadata-selector ()
  "Match and return the key sequence for a : selector."
  ())


(defun =atom-getter ()
  "Match and return the key sequence for an atom."
  (=list (=destructure (ns _)
           (%or (=list (=@-namespace)
                  (%maybe (?whitespace)))
                (=list (=atom-namespace)
                  (?whitespace))))
         (=msl-key)))
;

(defun =@-getter ()
  "Match and return the key sequence for an @."
  (=list (=destructure (ns _)
           (=list (=@-namespace)
                  (%maybe (?whitespace))))
         (=msl-key)))
;

(defun =subatomic-getter ()
 "Match and return : / [] d f # namespace."
  (%or (=metadata-getter)))

(defun =metadata-getter ()
 "Match and return key sequence for :."
  (=list (=metadata-namespace)
         (=msl-key)))

(defun =metadata-namespace ()
  "Match and return the : namespace."
  (=subseq (?eq #\:)))

;
(defun =tiny-atom ()
  "Match and return an atom."
  (=destructure (_ getter-setter _ _)
    (=list (?eq #\left_parenthesis)
           (%or (=list (=@-getter)
                       (=metadata-getter))
                (=destructure (atom _ sub)
                  (=list (=single-getter)
                         (?whitespace)
                         (=subatomic-getter))
                  (list atom sub))
                (=single-getter))
           (?eq #\right_parenthesis)
           (?end))))

;;

;;
;; Function-namespace definitions for recursive functions
;;

(setf (fdefinition '=msl-value/parser) (=msl-value)
      (fdefinition '=msl-atom/parser) (=msl-atom))
