;;;; expr.lisp

(uiop:define-package #:streams/expr
    (:use #:cl #:maxpc)
    (:export #:parse-msl #:parse-explain))

(in-package #:streams/expr)

;; Utility (Non-Parser) Functions

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


;; MSL Explainer System (MSLES)

(defun =msl-expression ()
  "Match and return an MSL expression."
  (%or '@-form
       'grouping-form
       'canon-form
       (=prelude-form)))
;;

(defun parse-msl (expr)
  "Parse an MSL expression."
  (parse expr (=msl-expression)))
;;

(defun parse-explain (expr)
  "Parse and explain an MSL expression."
  (let ((parsed-atom (parse-msl expr))
        (@-explainer '(atom-seq atom-value atom-mods metadata hash comment)))
      (explain (collate @-explainer parsed-atom))))
;;

(defun explain (item-list)
  "Show a printed explainer for a parsed MSL expression."
  (cond ((not item-list) nil)
        (t (format t "~% ~A~13T| ~15T ~S" (car (car item-list)) (car (cdr (car item-list)))) (explain (cdr item-list)))))
;;

(defun collate (&rest lists)
  "Combine the first item of each list, second item, etc."
  (apply #'mapcar #'list lists))
;;

;; STREAM (CHARACTER) PARSERS

;; TEST PARSERS (Don't return a value)

(defun ?whitespace ()
  "Match one or more whitespace characters."
  (?seq (%some (maxpc.char:?whitespace))))
;;

(defun ?hexp ()
  "Match a single hex character."
  (?satisfies 'hex-char-p))
;;

(defun ?value-terminator ()
  "Match the end of a value."
  (%or
       'nested-atom
       'metadata-sequence
       'regex-sequence
       'bracketed-transform-sequence
       'datatype-form
       'format-form
       'msl-hash
       'msl-comment
       (?seq (?eq #\right_parenthesis) 'metadata-sequence)
       (?seq (?eq #\right_parenthesis) 'datatype-form)
       (?seq (?eq #\right_parenthesis) 'format-form)
       (?seq (?eq #\right_parenthesis) (?eq #\right_parenthesis))
       (?seq (?eq #\right_parenthesis) (?end))))
;;

(defun ?expression-terminator ()
  "Match the end of an expression."
    (?seq
      (?eq #\right_parenthesis)))
;;

;; SINGLE VALUE PARSERS (Return one value.)

(defun =sha256 ()
  "Match and return a SHA-256 string."
  (=subseq (?seq (?satisfies 'length-64-p
                   (=subseq (%some (?hexp)))))))
;;

(defun =msl-key ()
  "Match and return a valid MSL key."
  (=subseq (?seq (%some (?satisfies 'alphanumericp))
                 (%any (?seq (%maybe (?eq #\-))
                         (%some (?satisfies 'alphanumericp)))))))
;;

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

(defun =msl-comment ()
 "Match a comment."
  (=destructure (_ _ comment)
    (=list (?whitespace)
           (maxpc.char:?string "//")
           (=subseq (%some (?not (%or (?expression-terminator))))))))
;;

;; Nested Expression Parsers

(defun =nested-@ ()
  "Match and return a nested atom."
  (=destructure (_ atom)
    (=list (?whitespace)
           '@-form)))
;;

(defun =nested-atom ()
  "Match and return a nested atom."
  (=destructure (_ atom)
    (=list (?whitespace)
           (%or '@-form
                'canon-form
                'grouping-form))))
;;

(defun =nested-group ()
  "Match and return a nested atom."
  (=destructure (_ atom)
    (=list (?whitespace)
           'grouping-form)))
;;

(defun =nested-canon ()
  "Match and return a nested atom."
  (=destructure (_ atom)
    (=list (?whitespace)
           'canon-form)))
;;

;; Namespace Parsers

(defun =@-namespace ()
    "Match and return the @ namespace."
    (=subseq (?eq #\@)))
;;

(defun =atom-namespace ()
  "Match and return an atom namespace."
  (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v #\c #\@))))))
;;

(defun =grouping-namespace ()
    "Match and return m w s v namespace."
    (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v))))))
;;

(defun =regex-namespace ()
    "Match and return the / namespace."
    (=subseq (?eq #\/)))
;;

(defun =prelude-namespace ()
    "Match and return the msl namespace."
    (=subseq (maxpc.char:?string "msl")))
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

(defun =format-namespace ()
        "Match and return the f namespace."
        (=subseq (?eq #\f)))
;;

;; LIST OF VALUES PARSERS (Return a list.)

;; Namespace Sequences

(defun =@-sequence ()
  "Match and return the key sequence for an @."
  (=list (=destructure (ns _)
           (=list (=@-namespace)
                  (%maybe (?whitespace))))
         (=msl-key)))
;;

(defun =grouping-sequence ()
  "Match and return the key sequence for an atom."
  (=list (=destructure (ns _)
           (=list (=grouping-namespace)
                  (?whitespace)))
         (=msl-key)))
;;

(defun =canon-sequence ()
  "Match and return the key sequence for canon."
  (=list (=destructure (ns _)
           (=list (=canon-namespace)
                  (?whitespace)))
         (=msl-key)))
;;

(defun =prelude-sequence ()
  "Match and return the key sequence for a prelude."
  (=list (=destructure (ns _)
           (=list (=prelude-namespace)
                  (?whitespace)))
         (=msl-key)))
;;

(defun =metadata-sequence ()
 "Match and return key sequence for : namespace."
  (=destructure (_ ns key)
    (=list (?whitespace)
           (=metadata-namespace)
           (=msl-key))
    (list ns key)))
;;

(defun =regex-sequence ()
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
                             (%maybe 'msl-value))
                      (list regex env value))))
                (cond (regex-list (list "/" regex-list)))))

;;

(defun =bracketed-transform-sequence ()
 "Match and return the key sequence for []."
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

(defun =datatype-sequence ()
  "Match and return key sequence for d."
  (=destructure (atom _ key)
    (=list (=datatype-namespace)
           (?whitespace)
           (=msl-key))
    (list atom key)))
;;

(defun =format-sequence ()
  "Match and return key sequence for f."
  (=destructure (atom _ key)
    (=list (=format-namespace)
           (?whitespace)
           (=msl-key))
    (list atom key)))
;;


;; Mods

(defun =atom-mods ()
  "Match and return key sequence for / [] d f namespace."
  (%or 'regex-sequence
       'datatype-form
       'format-form
       'bracketed-transform-sequence))
;;

(defun =format-mods ()
  "Match and return key sequence for / d f namespace."
  (%or 'regex-sequence
       'datatype-form
       'format-form))

(defun =prelude-form ()
   "Match and return an atom in the msl namespace."
   (let ((saved-val))
     (=destructure (_ atom-seq atom-value atom-mods metadata hash comment _)
                   (=list (?eq #\left_parenthesis)
                          (=prelude-sequence)
                          (=transform (%any 'msl-value)
                                      (lambda (val)
                                              (when val (setf saved-val val))))
                          (%any 'atom-mods)
                          (%maybe (%or
                                      (%some (=destructure (meta-seq meta-value meta-mods)
                                              (%or
                                                (=list 'metadata-sequence
                                                       (%some 'msl-value)
                                                       (%any 'atom-mods))
                                                (=list 'metadata-sequence
                                                       (%any 'msl-value)
                                                       (%some 'atom-mods)))
                                              (list meta-seq meta-value meta-mods)))
                                      (=destructure (meta-seq meta-value meta-mods)
                                        (=list 'metadata-sequence
                                               (?satisfies (lambda (val)
                                                                   (declare (ignore val)) (unless saved-val t))
                                                           (%maybe 'msl-value))
                                               (%any 'atom-mods))
                                        (list (list meta-seq meta-value meta-mods)))))
                          (%maybe 'msl-hash)
                          (%maybe 'msl-comment)
                          (?expression-terminator))
                   (list atom-seq atom-value atom-mods metadata hash comment))))
;;;;

;; Atom Forms

(defun =@-form ()
   "Match and return an atom in the @ namespace."
   (let ((saved-val))
     (=destructure (_ atom-seq atom-value atom-mods metadata hash comment _)
                   (=list (?eq #\left_parenthesis)
                          (=@-sequence)
                          (=transform (%any (%or 'nested-@
                                                 'nested-group
                                                 'msl-value))
                                      (lambda (val)
                                              (when val (setf saved-val val))))
                          (%any 'atom-mods)
                          (%maybe (%or
                                      (%some (=destructure (meta-seq meta-value meta-mods)
                                              (%or
                                                (=list 'metadata-sequence
                                                       (%some (%or 'nested-@
                                                                   'nested-group
                                                                   'msl-value))
                                                       (%any 'atom-mods))
                                                (=list 'metadata-sequence
                                                       (%any (%or 'nested-@
                                                                  'nested-group
                                                                  'msl-value))
                                                       (%some 'atom-mods)))
                                              (list meta-seq meta-value meta-mods)))
                                      (=destructure (meta-seq meta-value meta-mods)
                                        (=list 'metadata-sequence
                                               (?satisfies (lambda (val)
                                                                   (declare (ignore val)) (unless saved-val t))
                                                           (%maybe (%or 'nested-@
                                                                        'nested-group
                                                                        'msl-value)))
                                               (%any 'atom-mods))
                                        (list (list meta-seq meta-value meta-mods)))))
                          (%maybe 'msl-hash)
                          (%maybe 'msl-comment)
                          (?expression-terminator))
                   (list atom-seq atom-value atom-mods metadata hash comment))))
;;;;


(defun =grouping-form ()
   "Match and return an atom in the m w s v namespace."
   (let ((saved-val))
        (=destructure (_ atom-seq atom-value atom-mods metadata hash comment _)
                      (=list (?eq #\left_parenthesis)
                             'grouping-sequence
                             (=transform (%any (%or 'nested-atom
                                                    'msl-value))
                                         (lambda (val)
                                                 (when val (setf saved-val val))))
                             (%any 'atom-mods)
                             (%maybe (%or
                                         (%some (=destructure (meta-seq meta-value meta-mods)
                                                 (%or
                                                   (=list 'metadata-sequence
                                                          (%some (%or 'nested-atom
                                                                      'msl-value))
                                                          (%any 'atom-mods))
                                                   (=list 'metadata-sequence
                                                          (%any (%or 'nested-atom
                                                                     'msl-value))
                                                          (%some 'atom-mods)))
                                                 (list meta-seq meta-value meta-mods)))
                                         (=destructure (meta-seq meta-value meta-mods)
                                           (=list 'metadata-sequence
                                                  (?satisfies (lambda (val)
                                                                      (declare (ignore val)) (unless saved-val t))
                                                              (%maybe (%or 'nested-atom
                                                                           'msl-value)))
                                                  (%any 'atom-mods))
                                           (list (list meta-seq meta-value meta-mods)))))
                             (%maybe 'msl-hash)
                             (%maybe 'msl-comment)
                             (?expression-terminator))
                      (list atom-seq atom-value atom-mods metadata hash comment))))
;;

(defun =canon-form ()
   "Match and return an atom in the c namespace."
   (let ((saved-val))
     (=destructure (_ atom-seq atom-value atom-mods metadata hash comment _)
                   (=list (?eq #\left_parenthesis)
                          (=canon-sequence)
                          (=transform (%any (%or 'nested-@
                                                 'nested-canon
                                                 'msl-value))
                                      (lambda (val)
                                              (when val (setf saved-val val))))
                          (%any 'atom-mods)
                          (%maybe (%or
                                      (%some (=destructure (meta-seq meta-value meta-mods)
                                              (%or
                                                (=list 'metadata-sequence
                                                       (%some (%or 'nested-@
                                                                   'nested-canon
                                                                   'msl-value))
                                                       (%any 'atom-mods))
                                                (=list 'metadata-sequence
                                                       (%any (%or 'nested-@
                                                                  'nested-canon
                                                                  'msl-value))
                                                       (%some 'atom-mods)))
                                              (list meta-seq meta-value meta-mods)))
                                      (=destructure (meta-seq meta-value meta-mods)
                                        (=list 'metadata-sequence
                                               (?satisfies (lambda (val)
                                                                   (declare (ignore val)) (unless saved-val t))
                                                           (%maybe (%or 'nested-@
                                                                        'nested-canon
                                                                        'msl-value)))
                                               (%any 'atom-mods))
                                        (list (list meta-seq meta-value meta-mods)))))
                          (%maybe 'msl-hash)
                          (%maybe 'msl-comment)
                          (?expression-terminator))
                   (list atom-seq atom-value atom-mods metadata hash comment))))
;;


(defun =datatype-form ()
   "Match and return an atom in the d namespace."
   (=destructure (_ _ atom-seq atom-value atom-mods metadata comment _)
                 (=list (?whitespace)
                        (?eq #\left_parenthesis)
                        (=datatype-sequence)
                        (%maybe 'msl-value)
                        (%maybe 'regex-sequence)
                        (%maybe (%or
                                    (%some (=destructure (meta-seq meta-value meta-mods)
                                            (%or
                                              (=list 'metadata-sequence
                                                     'msl-value
                                                     (%any 'regex-sequence))
                                              (=list 'metadata-sequence
                                                     (%maybe 'msl-value)
                                                     (%some 'regex-sequence)))
                                            (list meta-seq meta-value meta-mods)))
                                    (=destructure (meta-seq meta-value meta-mods)
                                      (=list 'metadata-sequence
                                             (%maybe 'msl-value)
                                             (%any 'regex-sequence))
                                      (list meta-seq meta-value meta-mods))))
                        (%maybe 'msl-comment)
                        (?expression-terminator))
                 (list atom-seq atom-value atom-mods metadata NIL comment)))
;;
;;

(defun =format-form ()
   "Match and return an atom in the f namespace."
   (=destructure (_ _ atom-seq atom-value atom-mods metadata comment _)
                 (=list (?whitespace)
                        (?eq #\left_parenthesis)
                        (=format-sequence)
                        (%maybe 'msl-value)
                        (%maybe 'format-mods)
                        (%maybe (%or
                                    (%some (=destructure (meta-seq meta-value meta-mods)
                                            (%or
                                              (=list 'metadata-sequence
                                                     'msl-value
                                                     (%any 'format-mods))
                                              (=list 'metadata-sequence
                                                     (%maybe 'msl-value)
                                                     (%some 'format-mods)))
                                            (list meta-seq meta-value meta-mods)))
                                    (=destructure (meta-seq meta-value meta-mods)
                                      (=list 'metadata-sequence
                                             (%maybe 'msl-value)
                                             (%any 'format-mods))
                                      (list meta-seq meta-value meta-mods))))
                        (%maybe 'msl-comment)
                        (?expression-terminator))
                 (list atom-seq atom-value atom-mods metadata NIL comment)))
;;

;; Function-namespace definitions

(setf (fdefinition 'msl-value) (=msl-value)
      (fdefinition '@-form) (=@-form)
      (fdefinition 'grouping-form) (=grouping-form)
      (fdefinition 'canon-form) (=canon-form)
      (fdefinition 'datatype-form) (=datatype-form)
      (fdefinition 'format-form) (=format-form)
      (fdefinition 'regex-sequence) (=regex-sequence)
      (fdefinition 'bracketed-transform-sequence) (=bracketed-transform-sequence)
      (fdefinition 'metadata-sequence) (=metadata-sequence)
      (fdefinition 'grouping-sequence) (=grouping-sequence)
      (fdefinition 'atom-mods) (=atom-mods)
      (fdefinition 'format-mods) (=format-mods)
      (fdefinition 'nested-@) (=nested-@)
      (fdefinition 'nested-canon) (=nested-canon)
      (fdefinition 'nested-atom) (=nested-atom)
      (fdefinition 'msl-hash) (=msl-hash)
      (fdefinition 'msl-comment) (=msl-comment))
