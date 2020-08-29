;;;; parser.lisp

(uiop:define-package #:streams/parser
  (:use #:cl
        #:streams/common
        #:maxpc
        #:marie))

(in-package #:streams/parser)


;;--------------------------------------------------------------------------------------------------
;; utilities
;;--------------------------------------------------------------------------------------------------

(defun hex-char-p (char)
  "Return true if CHAR is valid hexadecimal character."
  (∨ (digit-char-p char)
     (char<= #\a char #\f)
     (char<= #\A char #\F)))

(defun length-64-p (value)
  "Return true if VALUE is 64 characters long."
  (length= value 64))

(defun regex-char-p (char)
  "Return true if CHAR is a valid regex character."
  (∨ (alphanumericp char)
     (mem char '(#\\ #\+ #\* #\^ #\? #\| #\$ #\.
                 #\left_parenthesis #\right_parenthesis
                 #\[ #\] #\{ #\}))))

(defun filespec-char-p (char)
  "Return true if CHAR is a valid regex character."
  (∨ (alphanumericp char)
     (mem char '(#\- #\. #\_ #\~ #\: #\/ #\? #\#
                 ;;#\[ #\]
                 #\( #\)
                 #\@ #\! #\$ #\& #\' #\* #\+
                 #\, #\; #\% #\=))))

(def pad-thing (value)
  "Conditionally add a trailing space to VALUE."
  (cond ((null value)
         value)
        ((∧ (stringp value)
            (∨ (length= value 0)
               (not (char= (end value) #\space))))
         (cat value " "))
        (t value)))

(def unpad-thing (value)
  "Conditionally remove space padding from VALUE."
  (cond ((null value)
         value)
        ((∧ (stringp value)
            (length>= value 1)
            (char= (end value) #\space))
         (string-right-trim '(#\space #\newline) value))
        (t value)))

(def pad-things (things)
  "Add conditional padding to items in THINGS."
  (flet* ((fn (args &optional acc)
            (cond ((null args) (nreverse acc))
                  ((not (null (cdr args)))
                   (fn (cdr args)
                       (cons (pad-thing (car args)) acc)))
                  ((null (cdr args))
                   (fn (cdr args)
                       (cons (unpad-thing (car args)) acc)))
                  (t (fn (cdr args)
                         (cons (car args) acc))))))
    (fn things)))

(def denull (value)
  "Remove the null items from VALUE."
  (remove-if #'null* value))


;;--------------------------------------------------------------------------------------------------
;; test parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser ?blackspace ()
    "Match zero or more whitespace character input."
    (?seq (%any (maxpc.char:?whitespace))))

  (def-parser =blackspace ()
    "Match zero or more whitespace character input."
    (=subseq (%any (maxpc.char:?whitespace))))

  (def-parser ?whitespace ()
    "Match one or more whitespace character input."
    (?seq (%some (maxpc.char:?whitespace))))

  (def-parser =whitespace ()
    "Match one or more whitespace character input."
    (=subseq (%some (maxpc.char:?whitespace))))

  (def-parser ?hexp ()
    "Match a single hexadecimal character input."
    (?satisfies 'hex-char-p))

  (def-parser ?untrue ()
    "Match falsehood."
    (?satisfies 'false))

  (def-parser =sha256 ()
    "Match and return a SHA-256 string."
    (=subseq (?seq (?satisfies 'length-64-p (=subseq (%some 'hexp))))))

  (def-parser =key ()
    "Match and return a valid MSL key."
    (=subseq (?seq (%some (?satisfies 'alphanumericp))
                   (%any (?seq (%maybe (?eq #\-))
                               (%some (?satisfies 'alphanumericp))))))))

;;--------------------------------------------------------------------------------------------------
;; ns parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =@-namespace ()
    "Match and return the @ ns."
    (=subseq (?eq #\@)))

  (def-parser =grouping-namespace ()
    "Match and return m w s v ns."
    (=subseq (?satisfies (λ (c) (member c '(#\m #\w #\s #\v))))))

  (def-parser =regex-namespace ()
    "Match and return the / ns."
    (=subseq (?eq #\/)))

  (def-parser =prelude-namespace ()
    "Match and return the MSL ns."
    (=transform (=subseq (?satisfies (λ (string)
                                       (cl-ppcre:scan "[mM][sS][lL]" string))
                                     (=subseq (%some (?satisfies 'alphanumericp)))))
                (λ (string)
                  (string-upcase string))))

  (def-parser =metadata-namespace ()
    "Match and return the : ns."
    (=subseq (?eq #\:)))

  (def-parser =c-namespace ()
    "Match and return the c ns."
    (=subseq (?eq #\c)))

  (def-parser =datatype-namespace ()
    "Match and return the d ns."
    (=subseq (?eq #\d)))

  (def-parser =format-namespace ()
    "Match and return the f ns."
    (=subseq (?eq #\f))))


;;--------------------------------------------------------------------------------------------------
;; ns sequences
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =@-sequence ()
    "Match and return the key sequence for an @."
    (=list (=destructure
               (ns _)
               (=list (=@-namespace)
                      (%maybe (?whitespace))))
           (=key)))

  (def-parser =grouping-sequence ()
    "Match and return the key sequence for an atom."
    (=list (=destructure
               (ns _)
               (=list (=grouping-namespace)
                      (?whitespace)))
           (=key)))

  (def-parser =c-sequence ()
    "Match and return the key sequence for canon."
    (=list (=destructure
               (ns _)
               (=list (=c-namespace)
                      (?whitespace)))
           (=key)))

  (def-parser =prelude-sequence ()
    "Match and return the key sequence for a prelude."
    (=list (=destructure
               (ns _)
               (=list (=prelude-namespace)
                      (?whitespace)))
           (=key)))

  (def-parser =metadata-sequence ()
    "Match and return key sequence for : ns."
    (=destructure
        (_ ns key)
        (=list (?blackspace)
               (=metadata-namespace)
               (=key))
      (list ns key)))

  (def-parser =datatype-sequence ()
    "Match and return key sequence for d."
    (=destructure
        (atom _ key)
        (=list (=datatype-namespace)
               (?whitespace)
               (=key))
      (list atom key)))

  (def-parser =format-sequence ()
    "Match and return key sequence for f."
    (=destructure
        (atom _ key)
        (=list (=format-namespace)
               (?whitespace)
               (=key))
      (list atom key))))


;;--------------------------------------------------------------------------------------------------
;; test parsers (don't return a value)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser ?left-parenthesis ()
    "Match a left parenthesis."
    (?eq #\left_parenthesis))

  (def-parser ?right-parenthesis ()
    "Match a right parenthesis."
    (?eq #\right_parenthesis))

  (def-parser ?expression-starter ()
    "Match the end of an expression."
    (?seq (?left-parenthesis)))

  (def-parser ?expression-terminator ()
    "Match the end of an expression."
    (?seq (?right-parenthesis)))

  (def-parser ?value-terminator ()
    "Match the end of a value."
    (%or 'literal-atom-form
         'metadata-sequence
         'regex-selector
         'bracketed-transform-selector
         'datatype-form
         'format-form
         'hash
         'comment
         (?seq (?right-parenthesis) 'metadata-sequence)
         (?seq (?right-parenthesis) 'datatype-form)
         (?seq (?right-parenthesis) 'format-form)
         (?seq (?right-parenthesis) (?right-parenthesis))
         (?seq (?right-parenthesis) (?end)))))


;;--------------------------------------------------------------------------------------------------
;; single-value parsers (return one value.)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =filespec ()
    "Match and return a URI filespec or URL."
    (=subseq (%some (%and (?satisfies 'filespec-char-p)
                          (?not (?eq #\]))))))

  (def-parser =hash ()
    "Match and return a hash value."
    (=destructure
        (_ ns hash)
        (=list (?whitespace)
               (=subseq (?eq #\#))
               (=sha256))
      (list (list ns) (list hash))))

  (def-parser =value ()
    "Match and return a raw value."
    (=subseq (%some (?not (?value-terminator)))))

  (def-parser =comment ()
    "Match a comment."
    (=destructure
        (_ _ comment)
        (=list (?whitespace)
               (maxpc.char:?string "//")
               (=subseq (%some (?not (%or (?expression-terminator)))))))))


;;--------------------------------------------------------------------------------------------------
;; nested expressions parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =literal-@-form ()
    "Match and return a nested atom."
    (%or 'literal-@-form))

  (def-parser =literal-c-form ()
    "Match and return a nested atom."
    (%or 'c-form))

  (def-parser =literal-grouping-form ()
    "Match and return a nested atom."
    (%or 'grouping-form))

  (def-parser =literal-atom-form ()
    "Match and return a nested atom."
    (%or 'literal-@-form
         'literal-c-form
         'literal-grouping-form)))


;;--------------------------------------------------------------------------------------------------
;; list of values parsers (return a list)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =@-value ()
    "Match and return a valid value for @."
    (%or (=value)
         'literal-atom-form))

  (def-parser =c-value ()
    "Match and return a valid value for c."
    (%or (=value)
         'literal-@-form
         'literal-c-form))

  (def-parser =grouping-value ()
    "Match and return a valid value for m w s v."
    (%or (=value)
         'literal-@-form
         'literal-c-form
         'literal-grouping-form))

  (def-parser =regex-selector ()
    "Match and return the key sequence for /."
    (=destructure
        (regex-list)
        (=list (%some
                (=destructure
                    (_ regex _ env _ value)
                    (=list (=regex-namespace)
                           (=subseq (%some (?satisfies 'regex-char-p)))
                           (=regex-namespace)
                           (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                           (?blackspace)
                           (%maybe (=value)))
                  (list regex env value))))
      (when regex-list
        (list (list "/") regex-list nil nil nil nil))))

  (def-parser =bracketed-transform-selector ()
    "Match and return the key sequence for []."
    (=destructure
        (transform-list)
        (=list (%some
                (=destructure
                    (_ url _)
                    (=list (?eq #\[)
                           (=filespec)
                           (?eq #\])))))
      (when transform-list
        (list (list "[]") transform-list nil nil nil nil)))))


;;--------------------------------------------------------------------------------------------------
;; mods
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =atom-mods-1 ()
    "Match and return key sequence for the the /, and [] nss."
    (%or 'regex-selector
         'bracketed-transform-selector))

  (def-parser =atom-mods-2 ()
    "Match and return key sequence for the d, and f nss."
    (%or 'datatype-form
         'format-form))

  (def-parser =format-mods ()
    "Match and return key sequence for the /, d, and f nss."
    (%or 'regex-selector
         'datatype-form
         'format-form))

  (def-parser =datatype-mods ()
    "Match and return key sequence for the / ns."
    (%or 'regex-selector)))


;;--------------------------------------------------------------------------------------------------
;; atom forms (single-setter)
;;--------------------------------------------------------------------------------------------------

(defmacro +sequence (sequence)
  "Define a variable capturing parser macro for sequences."
  `(=transform ,sequence
               (λ (seq)
                 ;; note: this unconditionaly sets the atom sequence
                 ;;(setf %atom-sequence seq)

                 ;; note: are these all the right conditions?
                 ;; note: it seems, that the conditions are not enough.
                 (cond ((null %meta-sequence)
                        (setf %atom-sequence seq))

                       ;; note: this fixes the ghosting bug, but
                       ;; yields the embedded atom in metadata bug
                       (t (setf %atom-sequence seq))))))

(defmacro +value (value)
  "Define a variable capturing parser macro for values."
  `(=transform (%any ,value)
               (λ (val)
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
  `(=transform (=destructure
                   (_ atom-mods)
                   (=list (=blackspace)
                          (=atom-mods-2)))
               (λ (terms)
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
  `(=transform (=destructure
                   (_ atom-mods)
                   (=list (=blackspace)
                          (=atom-mods-2)))
               (λ (terms)
                 (prefix-terms (append %atom-sequence %meta-sequence)
                               terms))))

(defmacro +metadata-mods ()
  "Define a parser for handling metadata mods."
  `(%or (+metadata-mods-1)
        (+metadata-mods-2)))

(defmacro +metadata-sequence ()
  "Define a variable capturing parser macro for metadata sequence"
  `(=transform (=destructure
                   (_ atom-mods)
                   (=list (=blackspace)
                          (=metadata-sequence)))
               (λ (seq)
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
        (meta-sequence _ meta-value meta-mods)
        (%or
         ;; a value, with zero or more metadata mods
         (=list (+metadata-sequence)
                (?blackspace)
                (%some ,value)
                (%any (+metadata-mods)))
         ;; zero or more values, with metadata mods
         (=list (+metadata-sequence)
                (?blackspace)
                (%any ,value)
                (%some (+metadata-mods)))
         ;; no atom value, zero or more metadata mods; the birthday trap
         (=list (+metadata-sequence)
                (?blackspace)
                (?satisfies (λ (_)
                              (declare (ignore _))
                              (unless %atom-value t))
                            (%any ,value))
                (%any (+metadata-mods))))
      (declare (ignore meta-sequence))
      (let* ((mods (red-append meta-mods))
             (value (build-items %atom-sequence %meta-sequence meta-value mods)))
        value))))

(defmacro +hash ()
  "Define a variable capturing parser macro for hash."
  `(=destructure
       (hash-seq hash-value)
       (=hash)
     (list (list (append %atom-sequence hash-seq) hash-value))))

(defmacro +@-metadata ()
  "Define a variable capturing parser macro for @ with a single abutted metadata recall."
  `(=destructure
       (_ atom-sequence metadata _ _ _)
       (=list (?expression-starter)
              (=@-sequence)
              (=metadata-sequence)
              (%maybe (+hash))
              (%maybe (=comment))
              (?expression-terminator))
     (list (list atom-sequence nil)
           (list (append atom-sequence metadata) nil))))

(defm def-parser-form (name sequence value)
  "Define a macro for defining parsers."
  (macrolet ((~@-metadata ()
               `(if (equal name '=@-form)
                    '(+@-metadata)
                    '(?untrue))))
    `(def-parser ,name ()
       (let ((%atom-value)
             (%atom-sequence)
             (%meta-sequence))
         (%or ,(~@-metadata)
              (=destructure
                  (_ atom-sequence _ atom-value atom-mods metadata hash _ _)
                  (=list (?expression-starter)
                         (+sequence ,sequence)
                         (?blackspace)
                         (+value ,value)
                         (%any (+atom-mods))
                         (%maybe (+metadata ,value))
                         (%maybe (+hash))
                         (%maybe (=comment))
                         (?expression-terminator))
                (let* ((head (list (list atom-sequence atom-value)))
                       (mods (red-append atom-mods))
                       (meta (red-append metadata))
                       (value (cond ((∧ (null atom-value)
                                        metadata)
                                     (red-append mods meta hash))
                                    (t (red-append head mods meta hash)))))
                  value)))))))

(def-parser-form =prelude-form (=prelude-sequence) (=value))
(def-parser-form =@-form (=@-sequence) (=@-value))
(def-parser-form =c-form (=c-sequence) (=c-value))
(def-parser-form =grouping-form (=grouping-sequence) (=grouping-value))
(def-parser-form =format-form (=format-sequence) (=value))
(def-parser-form =datatype-form (=datatype-sequence) (=value))


;;--------------------------------------------------------------------------------------------------
;; literal parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser ?literal-value-terminator ()
    "Define a literal parser for matching the end of a value."
    (macrolet ((~seq (&rest data)
                 `(?seq (?right-parenthesis) ,@data)))
      (%or 'literal-regex-selector
           'literal-bracketed-transform-selector
           'literal-datatype-form
           'literal-format-form
           'literal-hash
           'literal-comment
           (~seq 'literal-regex-selector)
           (~seq 'literal-bracketed-transform-selector)
           (~seq 'literal-datatype-form)
           (~seq 'literal-format-form)
           (~seq 'literal-hash)
           (~seq 'literal-comment)
           (~seq (%some (?right-parenthesis)))
           (~seq (?end))
           (~seq 'literal-value))))

  (def-parser =literal-value ()
    "Match and return a raw value."
    (=subseq (%some (?not (?literal-value-terminator)))))

  (def-parser =literal-regex-selector ()
    "Match and return the literal key sequence for /."
    (=destructure
        (&rest regex-list)
        (%some (=destructure
                   (_ regex _ env _ value)
                   (=list (=regex-namespace)
                          (=subseq (%some (?satisfies 'regex-char-p)))
                          (=regex-namespace)
                          (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                          (?blackspace)
                          (%maybe (=literal-value)))
                 (list regex env value)))
      (when regex-list
        (red-cat (pad-things (make-regex regex-list))))))

  (def-parser =literal-bracketed-transform-selector ()
    "Match and return the literal key sequence for []."
    (=destructure
        (transform-list)
        (=list (%some
                (=destructure
                    (_ url _ _)
                    (=list (?eq #\[)
                           (=filespec)
                           (?eq #\])
                           (?blackspace)))))
      (when transform-list
        (red-cat (pad-things (make-transform transform-list))))))

  (def-parser =literal-atom-mods-1 ()
    "Match and return key sequence for the the /, and [] nss."
    (%or 'literal-regex-selector
         'literal-bracketed-transform-selector))

  (def-parser =literal-atom-mods-2 ()
    "Match and return key sequence for the d, and f nss."
    (%or 'literal-datatype-form
         'literal-format-form))

  (def-parser =literal-atom-mods ()
    "Match and return atom mods."
    (%or (=literal-atom-mods-1)
         (=literal-atom-mods-2)))

  (def-parser =literal-hash ()
    "Match and return a hash value."
    (=destructure
        (ns hash)
        (=list (=subseq (?eq #\#))
               (=sha256))
      (cat ns hash)))

  (def-parser =literal-comment ()
    "Match a comment."
    (=destructure
        (_ comment)
        (=list (maxpc.char:?string "//")
               (=subseq (%some (?not (%or (?expression-terminator)))))))))

(def make-seq (ns key)
  "Return a key sequence from NS and KEY."
  (cond ((string= ns "@") (cat ns key))
        (t (cat ns " " key))))

(defm def-literal-parser-form (name ns value)
  "Define a macro for defining literal parsers."
  `(def-parser ,name ()
     (=destructure
         (_ ns _ key _ atom-value atom-mods hash _ _)
         (=list (?expression-starter)
                ,ns
                (%maybe (?whitespace))
                (=key)
                (?blackspace)
                (%maybe ,value)
                (%any (=literal-atom-mods))
                (%maybe (=literal-hash))
                (%maybe (=literal-comment))
                (?expression-terminator))
       (let* ((seq (make-seq ns key))
              (mods (red-cat (pad-things atom-mods)))
              (list (list seq atom-value mods hash))
              (value (denull list))
              (things (pad-things value)))
         (list-string* things)))))

(def-literal-parser-form =literal-@-form (=@-namespace) (=literal-value))
(def-literal-parser-form =literal-c-form (=c-namespace) (=literal-value))
(def-literal-parser-form =literal-grouping-form (=grouping-namespace) (=literal-value))
(def-literal-parser-form =literal-format-form (=format-namespace) (=literal-value))
(def-literal-parser-form =literal-datatype-form (=datatype-namespace) (=literal-value))

(def-parser =literal-expression ()
  "Match and return a literal MSL expression."
  (%or (=literal-@-form)
       (=literal-c-form)
       (=literal-grouping-form)
       (=literal-format-form)
       (=literal-datatype-form)))


;;--------------------------------------------------------------------------------------------------
;; top-level
;;--------------------------------------------------------------------------------------------------

(def-parser =expression ()
  "Match and return an MSL expression."
  (%or (=prelude-form)
       (=@-form)
       (=c-form)
       (=grouping-form)
       (=format-form)
       (=datatype-form)
       (=regex-selector)))

(def parse-msl (expr)
  "Parse an MSL expression."
  (parse expr (=expression)))

(def parse-setters (expr)
  "Parse an MSL expression and explain as MIL single-setters."
  (let ((parse (parse-msl expr)))
    (loop :for count :from 1
          :for term :in parse
          :do (format t "~A.~4T~S~%" count term))
    parse))

(def read-expr (value)
  "Return the parse of VALUE."
  (if (consp value)
      value
      (parse-msl value)))

(def exprp (value)
  "Return true if VALUE is a parseable expression."
  (etypecase value
    (string (∧ (parse-msl value)))
    (t nil)))
