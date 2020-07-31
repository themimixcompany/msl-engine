;;;; parser.lisp

(uiop:define-package #:streams/parser
  (:use #:cl
        #:streams/common
        #:maxpc
        #:marie)
  (:export #:parse-msl
           #:parse-setters
           #:parse-explain))

(in-package #:streams/parser)


;;--------------------------------------------------------------------------------------------------
;; utilities
;;--------------------------------------------------------------------------------------------------

(defun hex-char-p (char)
  "Return true if CHAR is valid hexadecimal character."
  (or (digit-char-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(defun length-64-p (value)
  "Return true if VALUE is 64 characters long."
  (length= value 64))

(defun regex-char-p (char)
  "Return true if CHAR is a valid regex character."
  (or (alphanumericp char)
      (mem char '(#\\ #\+ #\* #\^ #\? #\| #\$ #\.
                  #\left_parenthesis #\right_parenthesis
                  #\[ #\] #\{ #\}))))

(defun append-each (base-list item-list)
  "Append BASE-LIST to each item of ITEM-LIST."
  (cond ((null item-list) base-list)
        (t (append-each (append base-list (car item-list))
                        (cdr item-list)))))

(defun diag (&optional message value)
  "Print a diagnostic message."
  (format t "~%~5T>>> ~A ~A~%~%" message value))


;;--------------------------------------------------------------------------------------------------
;; test parsers (don't return a value)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser ?whitespace ()
    "Match one or more whitespace characters."
    (?seq (%some (maxpc.char:?whitespace))))

  (define-parser ?hexp ()
    "Match a single hex character."
    (?satisfies 'hex-char-p))

  (define-parser =sha256 ()
    "Match and return a SHA-256 string."
    (=subseq (?seq (?satisfies 'length-64-p
                               (=subseq (%some 'hexp))))))

  (define-parser =msl-key ()
    "Match and return a valid MSL key."
    (=subseq (?seq (%some (?satisfies 'alphanumericp))
                   (%any (?seq (%maybe (?eq #\-))
                               (%some (?satisfies 'alphanumericp))))))))

;;--------------------------------------------------------------------------------------------------
;; namespace parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =@-namespace ()
    "Match and return the @ namespace."
    (=subseq (?eq #\@)))

  (define-parser =atom-namespace ()
    "Match and return an atom namespace."
    (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v #\c #\@))))))

  (define-parser =grouping-namespace ()
    "Match and return m w s v namespace."
    (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v))))))

  (define-parser =regex-namespace ()
    "Match and return the / namespace."
    (=subseq (?eq #\/)))

  (define-parser =prelude-namespace ()
    "Match and return the msl namespace."
    (=subseq (maxpc.char:?string "msl")))

  (define-parser =metadata-namespace ()
    "Match and return the : namespace."
    (=subseq (?eq #\:)))

  (define-parser =canon-namespace ()
    "Match and return the c namespace."
    (=subseq (?eq #\c)))

  (define-parser =datatype-namespace ()
    "Match and return the d namespace."
    (=subseq (?eq #\d)))

  (define-parser =format-namespace ()
    "Match and return the f namespace."
    (=subseq (?eq #\f))))


;;--------------------------------------------------------------------------------------------------
;; namespace sequences
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =@-sequence ()
    "Match and return the key sequence for an @."
    (=list (=destructure
               (ns _)
               (=list (=@-namespace)
                      (%maybe (?whitespace))))
           (=msl-key)))

  (define-parser =grouping-sequence ()
    "Match and return the key sequence for an atom."
    (=list (=destructure
               (ns _)
               (=list (=grouping-namespace)
                      (?whitespace)))
           (=msl-key)))

  (define-parser =canon-sequence ()
    "Match and return the key sequence for canon."
    (=list (=destructure
               (ns _)
               (=list (=canon-namespace)
                      (?whitespace)))
           (=msl-key)))

  (define-parser =prelude-sequence ()
    "Match and return the key sequence for a prelude."
    (=list (=destructure
               (ns _)
               (=list (=prelude-namespace)
                      (?whitespace)))
           (=msl-key)))

  (define-parser =metadata-sequence ()
    "Match and return key sequence for : namespace."
    (=destructure
        (_ ns key)
        (=list (?whitespace)
               (=metadata-namespace)
               (=msl-key))
      (list ns key)))

  (define-parser =datatype-sequence ()
    "Match and return key sequence for d."
    (=destructure
        (atom _ key)
        (=list (=datatype-namespace)
               (?whitespace)
               (=msl-key))
      (list atom key)))

  (define-parser =format-sequence ()
    "Match and return key sequence for f."
    (=destructure
        (atom _ key)
        (=list (=format-namespace)
               (?whitespace)
               (=msl-key))
      (list atom key))))


;;--------------------------------------------------------------------------------------------------
;; test parsers (don't return a value)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser ?expression-starter ()
    "Match the end of an expression."
    (?seq (?eq #\left_parenthesis)))

  (define-parser ?expression-terminator ()
    "Match the end of an expression."
    (?seq (?eq #\right_parenthesis)))

  (define-parser ?value-terminator ()
    "Match the end of a value."
    (%or 'nested-atom
         (=metadata-sequence)
         'regex-selector
         'bracketed-transform-selector
         'datatype-form
         'format-form
         'msl-hash
         'msl-comment
         (?seq (?eq #\right_parenthesis) (=metadata-sequence))
         (?seq (?eq #\right_parenthesis) 'datatype-form)
         (?seq (?eq #\right_parenthesis) 'format-form)
         (?seq (?eq #\right_parenthesis) (?eq #\right_parenthesis))
         (?seq (?eq #\right_parenthesis) (?end)))))


;;--------------------------------------------------------------------------------------------------
;; single-value parsers (return one value.)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =msl-filespec ()
    "Match and return a URI filespec or URL."
    (=subseq (%some (?satisfies 'alphanumericp))))

  (define-parser =msl-hash ()
    "Match and return a hash value."
    (=destructure
        (_ ns hash)
        (=list (?whitespace)
               (=subseq (?eq #\#))
               (=sha256))
      (list (list ns) (list hash))))

  (define-parser =msl-value ()
    "Match and return a raw value."
    (%and (?not (?value-terminator))
          (=destructure
              (_ value)
              (=list (?whitespace)
                     (=subseq (%some (?not (?value-terminator))))))))

  (define-parser =msl-comment ()
    "Match a comment."
    (=destructure
        (_ _ comment)
        (=list (?whitespace)
               (maxpc.char:?string "//")
               (=subseq (%some (?not (%or (?expression-terminator)))))))))


;;--------------------------------------------------------------------------------------------------
;; nested expressions parsers
;; NOTE: the symbols used in this section must be quoted because they are forward references
;;--------------------------------------------------------------------------------------------------

(define-parser =nested-@ ()
  "Match and return a nested atom."
  (=destructure
      (_ atom)
      (=list (?whitespace)
             '@-form)))

(define-parser =nested-atom ()
  "Match and return a nested atom."
  (=destructure
      (_ atom)
      (=list (?whitespace)
             (%or '@-form
                  'canon-form
                  'grouping-form))))

(define-parser =nested-group ()
  "Match and return a nested atom."
  (=destructure
      (_ atom)
      (=list (?whitespace)
             'grouping-form)))

(define-parser =nested-canon ()
  "Match and return a nested atom."
  (=destructure
      (_ atom)
      (=list (?whitespace)
             'canon-form)))


;;--------------------------------------------------------------------------------------------------
;; list of values parsers (return a list)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =@-value ()
    "Match and return a valid value for @."
    (%or 'nested-@
         'nested-group
         (=msl-value)))

  (define-parser =c-value ()
    "Match and return a valid value for c."
    (%or 'nested-@
         'nested-canon
         (=msl-value)))

  (define-parser =group-value ()
    "Match and return a valid value for m w s v."
    (%or 'nested-@
         'nested-group
         'nested-canon
         (=msl-value)))

  (define-parser =regex-selector ()
    "Match and return the key sequence for /."
    (=destructure
        (regex-list)
        (=list (%some
                (=destructure
                    (_ _ regex _ env value)
                    (=list (?whitespace)
                           (=regex-namespace)
                           (=subseq (%some (?satisfies 'regex-char-p)))
                           (=regex-namespace)
                           (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                           (%maybe (=msl-value)))
                  (list regex env value))))
      (when regex-list
        (list (list "/") regex-list nil nil nil nil))))

  (define-parser =bracketed-transform-selector ()
    "Match and return the key sequence for []."
    (=destructure
        (transform-list)
        (=list (%some
                (=destructure
                    (_ _ url _)
                    (=list (?whitespace)
                           (?eq #\[)
                           (=msl-filespec)
                           (?eq #\])))))
      (when transform-list
        (list (list "[]") transform-list nil nil nil nil)))))


;;--------------------------------------------------------------------------------------------------
;; Mods
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =atom-mods ()
    "Match and return key sequence for / [] d f namespace."
    (%or 'regex-selector
         'datatype-form
         'format-form
         'bracketed-transform-selector))

  (define-parser =format-mods ()
    "Match and return key sequence for / d f namespace."
    (%or 'regex-selector
         'datatype-form
         'format-form))

  (define-parser =datatype-mods ()
    "Match and return key sequence for / namespace."
    (%or 'regex-selector)))


;;--------------------------------------------------------------------------------------------------
;; atom forms (single-setter)
;;--------------------------------------------------------------------------------------------------

(defmacro +sequence (sequence)
  "Define a parser macro for sequences."
  `(=transform ,sequence
               (lambda (seq)
                 (setf %atom-sequence seq))))

(defmacro +value (value)
  "Define a parser macro for values."
  `(=transform (%any ,value)
               (lambda (val)
                 (cond (val (setf %atom-value val))
                       (t (setf %atom-value nil))))))

(defmacro +mods ()
  "Define a parser macro for atom mods."
  `(=destructure
       (mod-sequence &optional mod-value mod-mods mod-meta mod-hash mod-comment)
       (=atom-mods)
     (list (append %atom-sequence mod-sequence)
           mod-value
           mod-mods mod-meta mod-hash mod-comment)))

(defmacro +metadata-mods ()
  "Define a parser macro for handling metadata mods"
  `(=destructure
       (mod-sequence mod-value mod-mods mod-meta mod-hash mod-comment)
       (=atom-mods)
     (list (append %atom-sequence %meta-sequence mod-sequence)
           mod-value
           mod-mods mod-meta mod-hash mod-comment)))

(defun build-items (atom-sequence meta-sequence meta-value meta-mods)
  "Return a list for +METADATA processing."
  (cons (list (append atom-sequence meta-sequence)
              meta-value)
        meta-mods))

(defmacro +metadata (value)
  "Define a parser macro for metadata."
  `(%maybe
    (%or
     ;; one or more metadata keys, each one having:
     (%some (=destructure
                (meta-sequence meta-value meta-mods)
                (%or
                 ;; a value, maybe mods (META 1, the "value" case.)
                 ;; %some value + %any mods
                 (=list (=transform
                         (=metadata-sequence)
                         (lambda (seq)
                           (setf %meta-sequence seq)))
                        (%some ,value)
                        (%any (+metadata-mods)))
                 ;; no value, with mods (META 2, the "no value" case.)
                 ;; %any value + %some mods
                 (=list (=transform
                         (=metadata-sequence)
                         (lambda (seq)
                           (setf %meta-sequence seq)))
                        (%any ,value)
                        (%some (+metadata-mods))))
              (build-items %atom-sequence %meta-sequence meta-value meta-mods)))
     ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
     ;; %maybe value (if no %atom-value) + %any mods
     (=destructure
         (meta-sequence meta-value meta-mods)
         (=list (=transform
                 (=metadata-sequence)
                 (lambda (seq)
                   (setf %meta-sequence seq)))
                (?satisfies (lambda (val)
                              (declare (ignore val))
                              (unless %atom-value t))
                            (%any ,value))
                (%any (+metadata-mods)))
       (list (build-items %atom-sequence %meta-sequence meta-value meta-mods))))))

(defmacro +hash ()
  "Define a parser macro for hash."
  `(=destructure
       (hash-seq hash-value)
       (=msl-hash)
     (list (list (append %atom-sequence hash-seq) hash-value))))

(defmacro define-parser* (name sequence value)
  "Define a macro for defining parsers."
  `(define-parser ,name ()
     (let ((%atom-value)
           (%atom-sequence)
           (%meta-sequence))
       (=destructure
           (_ atom-sequence atom-value atom-mods metadata hash _ _)
           (=list (?expression-starter)
                  (+sequence ,sequence)
                  (+value ,value)
                  (%any (+mods))
                  (%maybe (+metadata ,value))
                  (%maybe (+hash))
                  (%maybe (=msl-comment))
                  (?expression-terminator))
         (append (append-each (append (list (list atom-sequence atom-value)) atom-mods)
                              metadata)
                 hash)))))

(define-parser* =@-form (=@-sequence) (=@-value))
(define-parser* =canon-form (=canon-sequence) (=c-value))
(define-parser* =grouping-form (=grouping-sequence) (=group-value))
(define-parser* =prelude-form (=prelude-sequence) (=msl-value))
;; (define-parser* =format-form (=format-sequence) (=msl-value))
;; (define-parser* =datatype-form (=datatype-sequence) (=msl-value))

(define-parser =format-form ()
  "Match and return an atom in the f namespace."
  (let ((saved-val))
    (=destructure
        (_ _ atom-seq atom-value atom-mods metadata comment _)
        (=list (?whitespace)
               (?eq #\left_parenthesis)
               (=format-sequence)
               (=transform (%any (=msl-value))
                           (lambda (val)
                             (cond (val (setf saved-val val))
                                   (t (setf saved-val nil)))))
               (%maybe (=format-mods))
               (%maybe (%or
                        (%some (=destructure
                                   (meta-seq meta-value meta-mods)
                                   (%or
                                    (=list (=metadata-sequence)
                                           (=msl-value)
                                           (%any (=format-mods)))
                                    (=list (=metadata-sequence)
                                           (%maybe (=msl-value))
                                           (%some (=format-mods))))
                                 (list meta-seq meta-value meta-mods)))
                        (=destructure
                            (meta-seq meta-value meta-mods)
                            (=list (=metadata-sequence)
                                   (?satisfies (lambda (val)
                                                 (declare (ignore val)) (unless saved-val t))
                                               (%maybe (=msl-value)))
                                   (%any (=format-mods)))
                          (list meta-seq meta-value meta-mods))))
               (%maybe (=msl-comment))
               (?expression-terminator))
      (list atom-seq atom-value atom-mods metadata nil comment))))

(define-parser =datatype-form ()
  "Match and return an atom in the d namespace."
  (let ((atom-val))
    (=destructure
        (_ _ atom-seq atom-value atom-mods metadata comment _)
        (=list (?whitespace)
               (?eq #\left_parenthesis)
               (=datatype-sequence)
               (=transform (%any (=msl-value))
                           (lambda (val)
                             (cond (val (setf atom-val val))
                                   (t (setf atom-val nil)))))
               (%maybe (=datatype-mods))
               (%maybe (%or (%some (=destructure
                                       (meta-seq meta-value meta-mods)
                                       (%or (=list (=metadata-sequence)
                                                   (=msl-value)
                                                   (%any (=datatype-mods)))
                                            (=list (=metadata-sequence)
                                                   (%maybe (=msl-value))
                                                   (%some (=datatype-mods))))
                                     (list meta-seq meta-value meta-mods)))
                            (=destructure
                                (meta-seq meta-value meta-mods)
                                (=list (=metadata-sequence)
                                       (?satisfies (lambda (val)
                                                     (declare (ignore val))
                                                     (unless atom-val t))
                                                   (%maybe (=msl-value)))
                                       (%any (=datatype-mods)))
                              (list meta-seq meta-value meta-mods))))
               (%maybe (=msl-comment))
               (?expression-terminator))
      (list atom-seq atom-value atom-mods metadata nil comment))))

(defun setup (value)
  "Return a value suitable for =@-FORM from the shorter version of =DATATYPE-FORM"
  (destructuring-bind (atom-seq atom-value atom-mods metadata hash comment)
      value
    (declare (ignorable atom-value atom-mods metadata hash comment))
    (list (list atom-seq atom-value)
          (when atom-mods
            (destructuring-bind (head &rest body)
                atom-mods
              (declare (ignorable body))
              (append (list (append atom-seq head) body))))
          (when metadata
            (loop :for meta :in metadata
                  :collect (destructuring-bind (path &rest body)
                               meta
                             (cons (append atom-seq path) body))))
          nil
          nil)))

(defun setup-print (value)
  "Display the results of applying SETUP to VALUE."
  (format t "~{~S~%~}" (setup value)))

(define-parser =msl-expression ()
  "Match and return an MSL expression."
  (%or (=@-form)
       (=grouping-form)
       (=canon-form)
       (=prelude-form)
       (=datatype-form)
       (=format-form)
       (=regex-selector)))

(defun parse-msl (expr)
  "Parse an MSL expression."
  (parse expr (=msl-expression)))

(defun explain-lines (setters &optional (line-num 1))
  "Print each setter from a list on a separate line."
  (cond ((not setters) nil)
        (t (format t "~A.~4T~S~%" line-num (car setters))
           (explain-lines (cdr setters) (+ line-num 1))
           t)))

(defun parse-setters (expr)
  "Parse an MSL expression and explain as MIL single-setters."
  (format t "~%")
  (let ((parsed-atom (parse-msl expr))
        (atom-explainer '(atom-seq atom-value atom-mods metadata hash comment)))
    (when (explain-lines parsed-atom) parsed-atom)))
