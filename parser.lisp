;;;; parser.lisp

(uiop:define-package #:streams/parser
  (:use #:cl
        #:streams/common
        #:maxpc
        #:marie)
  (:export #:parse-msl
           #:parse-setters))

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
                  #\( #\)
                  #\[ #\] #\{ #\}))))


;;--------------------------------------------------------------------------------------------------
;; test parsers
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
    (=subseq (?seq (?satisfies 'length-64-p (=subseq (%some 'hexp))))))

  (define-parser =key ()
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

  (define-parser =c-namespace ()
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
           (=key)))

  (define-parser =grouping-sequence ()
    "Match and return the key sequence for an atom."
    (=list (=destructure
               (ns _)
               (=list (=grouping-namespace)
                      (?whitespace)))
           (=key)))

  (define-parser =c-sequence ()
    "Match and return the key sequence for canon."
    (=list (=destructure
               (ns _)
               (=list (=c-namespace)
                      (?whitespace)))
           (=key)))

  (define-parser =prelude-sequence ()
    "Match and return the key sequence for a prelude."
    (=list (=destructure
               (ns _)
               (=list (=prelude-namespace)
                      (?whitespace)))
           (=key)))

  (define-parser =metadata-sequence* ()
    "Match and return key sequence for : namespace, without a leading whitespace"
    (=destructure
        (ns key)
        (=list (=metadata-namespace)
               (=key))
      (list ns key)))

  (define-parser =metadata-sequence ()
    "Match and return key sequence for : namespace."
    (=destructure
        (_ seq)
        (=list (?whitespace)
               (=metadata-sequence*))
      seq))

  (define-parser =datatype-sequence ()
    "Match and return key sequence for d."
    (=destructure
        (atom _ key)
        (=list (=datatype-namespace)
               (?whitespace)
               (=key))
      (list atom key)))

  (define-parser =format-sequence ()
    "Match and return key sequence for f."
    (=destructure
        (atom _ key)
        (=list (=format-namespace)
               (?whitespace)
               (=key))
      (list atom key)))


  ;;--------------------------------------------------------------------------------------------------
  ;; test parsers (don't return a value)
  ;;--------------------------------------------------------------------------------------------------

  (eval-always
    (define-parser ?expression-starter ()
      "Match the end of an expression."
      (?seq (?eq #\()))

    (define-parser ?expression-terminator ()
      "Match the end of an expression."
      (?seq (?eq #\))))

    (define-parser ?value-terminator ()
      "Match the end of a value."
      (%or 'nested-atom
           'metadata-sequence
           'regex-selector
           'bracketed-transform-selector
           'datatype-form
           'format-form
           'hash
           'comment
           (?seq (?eq #\)) 'nested-atom)
           (?seq (?eq #\)) 'metadata-sequence)
           (?seq (?eq #\)) 'regex-selector)
           (?seq (?eq #\)) 'bracketed-transform-selector)
           (?seq (?eq #\)) 'datatype-form)
           (?seq (?eq #\)) 'format-form)
           (?seq (?eq #\)) 'hash)
           (?seq (?eq #\)) 'comment)
           (?seq (?eq #\)) (?eq #\)))
           (?seq (?eq #\)) (?end)))))


  ;;--------------------------------------------------------------------------------------------------
  ;; single-value parsers (return one value.)
  ;;--------------------------------------------------------------------------------------------------

  (eval-always
    (define-parser =filespec ()
      "Match and return a URI filespec or URL."
      (=subseq (%some (?satisfies 'alphanumericp))))

    (define-parser =hash ()
      "Match and return a hash value."
      (=destructure
          (_ ns hash)
          (=list (?whitespace)
                 (=subseq (?eq #\#))
                 (=sha256))
        (list (list ns) (list hash))))

    (define-parser =value ()
      "Match and return a raw value."
      (%and (?not (?value-terminator))
            (=destructure
                (_ value)
                (=list (?whitespace)
                       (=subseq (%some (?not (?value-terminator))))))))

    (define-parser =comment ()
      "Match a comment."
      (=destructure
          (_ _ comment)
          (=list (?whitespace)
                 (maxpc.char:?string "//")
                 (=subseq (%some (?not (%or (?expression-terminator))))))))))


;;--------------------------------------------------------------------------------------------------
;; nested expressions parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
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
                    'c-form
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
               'c-form))))


;;--------------------------------------------------------------------------------------------------
;; list of values parsers (return a list)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =@-value ()
    "Match and return a valid value for @."
    (%or 'nested-@
         'nested-group
         (=value)))

  (define-parser =c-value ()
    "Match and return a valid value for c."
    (%or 'nested-@
         'nested-canon
         (=value)))

  (define-parser =grouping-value ()
    "Match and return a valid value for m w s v."
    (%or 'nested-@
         'nested-group
         'nested-canon
         (=value)))

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
                           (%maybe (=value)))
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
                           (=filespec)
                           (?eq #\])))))
      (when transform-list
        (list (list "[]") transform-list nil nil nil nil)))))


;;--------------------------------------------------------------------------------------------------
;; mods
;;--------------------------------------------------------------------------------------------------

(eval-always
  (define-parser =atom-mods-1 ()
    "Match and return key sequence for the the /, and [] namespaces."
    (%or 'regex-selector
         'bracketed-transform-selector))

  (define-parser =atom-mods-2 ()
    "Match and return key sequence for the d, and f namespaces."
    (%or 'datatype-form
         'format-form))

  (define-parser =format-mods ()
    "Match and return key sequence for the /, d, and f namespaces."
    (%or 'regex-selector
         'datatype-form
         'format-form))

  (define-parser =datatype-mods ()
    "Match and return key sequence for the / namespace."
    (%or 'regex-selector)))


;;--------------------------------------------------------------------------------------------------
;; atom forms (single-setter)
;;--------------------------------------------------------------------------------------------------

(defmacro +sequence (sequence)
  "Define a variable capturing parser macro for sequences."
  `(=transform ,sequence
               (lambda (seq)
                 (setf %atom-sequence seq))))

(defmacro +value (value)
  "Define a variable capturing parser macro for values."
  `(=transform (%any ,value)
               (lambda (val)
                 (cond (val (setf %atom-value val))
                       (t (setf %atom-value nil))))))

(defmacro +atom-mods-1 ()
  "Define a variable capturing parser macro for type 1 atom mods."
  `(=destructure
       (mod-sequence &optional mod-value mod-mods mod-meta mod-hash mod-comment)
       (=atom-mods-1)
     (list (list (append %atom-sequence mod-sequence)
                 mod-value mod-mods mod-meta mod-hash mod-comment))))

(defmacro +atom-mods-2 ()
  "Define a variable capturing parser macro for type 2 atom mods."
  `(=transform (=atom-mods-2)
               (lambda (terms)
                 (let ((value (prefix-terms %atom-sequence terms)))
                   value))))

(defmacro +atom-mods ()
  "Define a parser for handling atom mods."
  `(%or (+atom-mods-1)
        (+atom-mods-2)))

(defmacro +metadata-mods-1 ()
  "Define a variable capturing parser macro for type 1 metadata mods."
  `(=destructure
       (mod-sequence &optional mod-value mod-mods mod-meta mod-hash mod-comment)
       (=atom-mods-1)
     (list (list (append %atom-sequence %meta-sequence mod-sequence)
           mod-value mod-mods mod-meta mod-hash mod-comment))))

(defmacro +metadata-mods-2 ()
  "Define a variable capturing parser macro for type 2 metadata mods."
  `(=transform (=atom-mods-2)
               (lambda (terms)
                 (prefix-terms (append %atom-sequence %meta-sequence)
                               terms))))

(defmacro +metadata-mods ()
  "Define a parser for handling metadata mods."
  `(%or (+metadata-mods-1)
        (+metadata-mods-2)))

(defmacro +metadata-sequence ()
  "Define a variable capturing parser macro for metadata sequence"
  `(=transform (=metadata-sequence)
               (lambda (seq)
                 (setf %meta-sequence seq))))

(defun build-items (atom-sequence meta-sequence meta-value meta-mods)
  "Return a list for +METADATA processing."
  (cons (list (append atom-sequence meta-sequence)
              meta-value)
        meta-mods))

(defmacro +metadata (value)
  "Define a variable capturing parser macro for metadata."
  `(%some
    (=destructure
        (meta-sequence meta-value meta-mods)
        (%or
         ;; a value, with zero or more metadata mods
         (=list (+metadata-sequence)
                (%some ,value)
                (%any (+metadata-mods)))
         ;; zero or more values, with metadata mods
         (=list (+metadata-sequence)
                (%any ,value)
                (%some (+metadata-mods)))
         ;; no atom value, zero or more metadata mods; the birthday trap
         (=list (+metadata-sequence)
                (?satisfies (lambda (_)
                              (declare (ignore _))
                              (unless %atom-value t))
                            (%any ,value))
                (%any (+metadata-mods))))
      (declare (ignore meta-sequence))
      (let* ((mods (reduce-append meta-mods))
             (value (build-items %atom-sequence %meta-sequence meta-value mods)))
        value))))

(defmacro +hash ()
  "Define a variable capturing parser macro for hash."
  `(=destructure
       (hash-seq hash-value)
       (=hash)
     (list (list (append %atom-sequence hash-seq) hash-value))))

(defmacro define-parser-form (name sequence value)
  "Define a macro for defining parsers."
  (let ((modp (mem sequence '((=format-sequence)
                              (=datatype-sequence)))))
    (macrolet ((ml (symbol-1 symbol-2)
                 `(if modp
                      (list ',symbol-1 ',symbol-2)
                      (list ',symbol-2))))
      `(define-parser ,name ()
         (let ((%atom-value)
               (%atom-sequence)
               (%meta-sequence))
           (%or
            (=destructure
                (_ atom-sequence metadata hash _ _)
                (=list (?expression-starter)
                       (=@-sequence)
                       (=metadata-sequence*)
                       (%maybe (+hash))
                       (%maybe (=comment))
                       (?expression-terminator))
              (declare (ignore hash))
              (list (list atom-sequence nil)
                    (list (append atom-sequence metadata) nil)))
            (=destructure
               (,@(ml _ _) atom-sequence atom-value atom-mods metadata hash _ _)
               (=list ,@(ml (?whitespace) (?expression-starter))
                      (+sequence ,sequence)
                      (+value ,value)
                      (%any (+atom-mods))
                      (%maybe (+metadata ,value))
                      (%maybe (+hash))
                      (%maybe (=comment))
                      (?expression-terminator))
             (let* ((head (list (list atom-sequence atom-value)))
                    (mods (reduce-append atom-mods))
                    (meta (reduce-append metadata))
                    (value (reduce-append head mods meta hash)))
               value))))))))

(define-parser-form =@-form (=@-sequence) (=@-value))
(define-parser-form =c-form (=c-sequence) (=c-value))
(define-parser-form =grouping-form (=grouping-sequence) (=grouping-value))
(define-parser-form =prelude-form (=prelude-sequence) (=value))
(define-parser-form =format-form (=format-sequence) (=value))
(define-parser-form =datatype-form (=datatype-sequence) (=value))

(define-parser =expression ()
  "Match and return an MSL expression."
  (%or (=@-form)
       (=grouping-form)
       (=c-form)
       (=prelude-form)
       (=datatype-form)
       (=format-form)
       (=regex-selector)))

(defun parse-msl (expr)
  "Parse an MSL expression."
  (parse expr (=expression)))

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
    (when (explain-lines parsed-atom)
      parsed-atom)))
