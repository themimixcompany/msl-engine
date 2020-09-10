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
        (>= code #x7F))))

(defun in-char-range-p (char start &optional end)
  "Return true if the code of CHAR is within START and END, inclusively."
  (inp (char-code char) start end))

(defun whitespace-char-p (char)
  "Return true if CHAR is one of the whitespace characters."
  (mem char '(#\space #\tab #\newline #\page #\linefeed #\return #\rubout)))

(defun regex-char-p (char)
  "Return true if CHAR is a valid regex character."
  (∨ (alphanumericp char)
     (whitespace-char-p char)
     (in-char-range-p char #x21 #x22)   ; #\! #\"
     (in-char-range-p char #x23 #x29)   ; #\# #\$ #\% #\& #\' #\( #\)

     ;; excludes #x2F #\/
     (in-char-range-p char #x2A #x2E)   ; #\* #\+ #\, #\- #\.

     (in-char-range-p char #x3A #x40)   ; #\: #\; #\< #\= #\> #\? #\@
     (in-char-range-p char #x5B #x60)   ; #\[ #\\ #\] #\^ #\_ #\`
     (in-char-range-p char #x7B #x7E)   ; #\{ #\| #\} #\~
     (>= (char-code char) #x7F)))

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

(defmacro ~mod (symbol-1 symbol-2)
  "Define a capturing helper macro for handling format and datatype mods."
  `(if (mem sequence '((=format-sequence)
                       (=datatype-sequence)))
       (list ',symbol-1 ',symbol-2)
       (list ',symbol-2)))


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
    (=subseq (?satisfies (λ (string)
                           (cl-ppcre:scan "[mM][sS][lL]" string))
                         (=subseq (%some (?satisfies 'alphanumericp))))))

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
    (%or 'nested-expression
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
         (?seq (?right-parenthesis) (%any (?right-parenthesis)))
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
;; list of values parsers (return a list)
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser =@-value ()
    "Match and return a valid value for @."
    (%or 'nested-@-form
         'nested-grouping-form
         (=value)))

  (def-parser =c-value ()
    "Match and return a valid value for c."
    (%or 'nested-@-form
         'nested-c-form
         (=value)))

  (def-parser =grouping-value ()
    "Match and return a valid value for m w s v."
    (%or 'nested-@-form
         'nested-c-form
         'nested-grouping-form
         (=value)))

  (def-parser =regex-selector ()
    "Match and return the key sequence for /."
    (=destructure
        (regex-list)
        (=list (%some
                (=destructure
                    (_ _ regex _ env _ value)
                    (=list (?blackspace)
                           (=regex-namespace)
                           (=subseq (%some (?satisfies 'regex-char-p)))
                           (=regex-namespace)
                           (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                           (?blackspace)
                           (%maybe (=value)))
                  (list regex env value))))
      (when regex-list
        (list '("/") regex-list))))

  (def-parser =bracketed-transform-selector ()
    "Match and return the key sequence for []."
    (=destructure
        (transform-list)
        (=list (%some
                (=destructure
                    (_ _ url _)
                    (=list (?blackspace)
                           (?eq #\[)
                           (=filespec)
                           (?eq #\])))))
      (when transform-list
        (list '("[]") transform-list)))))


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
       (mod-sequence &optional mod-value)
       (=atom-mods-1)
     (list (list (append %atom-sequence mod-sequence)
                 mod-value))))

(defmacro +atom-mods-2 ()
  "Define a variable capturing parser macro for type 2 atom mods."
  `(=transform (=destructure
                   (_ atom-mods)
                   (=list (?blackspace)
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
       (mod-sequence &optional mod-value)
       (=atom-mods-1)
     (list (list (append %atom-sequence %meta-sequence mod-sequence)
                 mod-value))))

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
        (_ meta-sequence _ meta-value meta-mods)
        (%or
         ;; a value, with zero or more metadata mods
         (=list (%maybe (?whitespace))
                (+metadata-sequence)
                (?blackspace)
                (%some ,value)
                (%any (+metadata-mods)))
         ;; zero or more values, with metadata mods
         (=list (%maybe (?whitespace))
                (+metadata-sequence)
                (?blackspace)
                (%any ,value)
                (%some (+metadata-mods)))
         ;; no atom value, zero or more metadata mods; the birthday trap
         (=list (%maybe (?whitespace))
                (+metadata-sequence)
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

(defm +@-metadata ()
  "Define a variable capturing parser macro for @ with a single abutted metadata recall or set."
  `(=destructure
       (_ atom-sequence meta-sequence _ meta-value _ _ _)
       (=list (?expression-starter)
              (=@-sequence)
              (=metadata-sequence)
              (%maybe (?whitespace))
              (%maybe (=@-value))
              (%maybe (+hash))
              (%maybe (=comment))
              (?expression-terminator))
     (let ((value (if meta-value (list meta-value) nil)))
       (list (list atom-sequence nil)
             (list (append atom-sequence meta-sequence)
                   value)))))
(defmacro ~@-metadata ()
  "Define a capturing helper macro for handling abutted metadata in @ forms."
  `(if (equal name '=@-form)
       '(+@-metadata)
       '(?untrue)))

(defm def-parser-form (name sequence value)
  "Define a macro for defining parsers."
  `(def-parser ,name ()
     (let ((%atom-value)
           (%atom-sequence)
           (%meta-sequence))
       (%or ,(~@-metadata)
            (=destructure
                (,@(~mod _ _) atom-sequence _ atom-value atom-mods metadata hash _ _)
                (=list ,@(~mod (?blackspace) (?expression-starter))
                       (+sequence ,sequence)
                       (%maybe (?whitespace))
                       (+value ,value)
                       (%any (+atom-mods))
                       (%maybe (+metadata ,value))
                       (%maybe (+hash))
                       (%maybe (=comment))
                       (?expression-terminator))
              (let* ((head (list (list atom-sequence atom-value)))
                     (mods (red-append atom-mods))
                     (meta (red-append metadata))
                     (value (red-append head mods meta hash)))
                value))))))

(def-parser-form =prelude-form (=prelude-sequence) (=value))
(def-parser-form =@-form (=@-sequence) (=@-value))
(def-parser-form =c-form (=c-sequence) (=c-value))
(def-parser-form =grouping-form (=grouping-sequence) (=grouping-value))
(def-parser-form =format-form (=format-sequence) (=value))
(def-parser-form =datatype-form (=datatype-sequence) (=value))


;;--------------------------------------------------------------------------------------------------
;; nested parsers
;;--------------------------------------------------------------------------------------------------

(eval-always
  (def-parser ?nested-value-terminator ()
    "Define a nested parser for matching the end of a value."
    (%or 'nested-metadata-sequence
         'nested-regex-selector
         'nested-bracketed-transform-selector
         'nested-datatype-form
         'nested-format-form
         'nested-hash
         'nested-comment
         (?seq (?right-parenthesis) 'nested-value)
         (?seq (?right-parenthesis) 'nested-metadata-sequence)
         (?seq (?right-parenthesis) 'nested-datatype-form)
         (?seq (?right-parenthesis) 'nested-format-form)
         (?seq (?right-parenthesis) (%any (?right-parenthesis)))
         (?seq (?right-parenthesis) (?end))))

  (def-parser =nested-@-value ()
    "Match and return a valid value for @."
    (%or 'nested-@-form
         'nested-grouping-form
         'nested-value))

  (def-parser =nested-c-value ()
    "Match and return a valid value for c."
    (%or 'nested-@-form
         'nested-c-form
         'nested-value))

  (def-parser =nested-grouping-value ()
    "Match and return a valid value for m w s v."
    (%or 'nested-@-form
         'nested-c-form
         'nested-grouping-form
         'nested-value))

  (def-parser =nested-value ()
    "Match and return a raw value."
    (=subseq (%some (?not (?nested-value-terminator)))))

  (def-parser =nested-regex-selector ()
    "Match and return the nested key sequence for /."
    (=destructure
        (&rest regex-list)
        (%some (=destructure
                   (_ regex _ env _ value)
                   (=list (=regex-namespace)
                          (=subseq (%some (?satisfies 'regex-char-p)))
                          (=regex-namespace)
                          (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                          (?blackspace)
                          (%maybe (=nested-value)))
                 (list regex env value)))
      (when regex-list
        (red-cat (pad-things (make-regex regex-list))))))

  (def-parser =nested-bracketed-transform-selector ()
    "Match and return the nested key sequence for []."
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

  (def-parser =nested-metadata-sequence ()
    "Match and return key sequence for nested : ns."
    (=destructure
        (_ ns key)
        (=list (?blackspace)
               (=metadata-namespace)
               (=key))
      (cat ns key)))

  (def-parser =nested-atom-mods-1 ()
    "Match and return key sequence for the the /, and [] nss."
    (%or 'nested-regex-selector
         'nested-bracketed-transform-selector))

  (def-parser =nested-atom-mods-2 ()
    "Match and return key sequence for the d, and f nss."
    (%or 'nested-datatype-form
         'nested-format-form))

  (def-parser =nested-atom-mods ()
    "Match and return atom mods."
    (%or (=nested-atom-mods-1)
         (=nested-atom-mods-2)))

  (def-parser =nested-hash ()
    "Match and return a hash value."
    (=destructure
        (ns hash)
        (=list (=subseq (?eq #\#))
               (=sha256))
      (cat ns hash)))

  (def-parser =nested-comment ()
    "Match a comment."
    (=destructure
        (_ comment)
        (=list (maxpc.char:?string "//")
               (=subseq (%some (?not (%or (?expression-terminator))))))))

  (def-parser =nested-metadata-mods ()
    "Define a nested parser for handling metadata mods."
    (%or 'nested-atom-mods-1
         'nested-atom-mods-2))

  (def-parser =nested-metadata ()
    "Define a nested parser macro for metadata."
    (%some
     (=destructure
         (_ meta-sequence _ meta-value meta-mods)
         (%or
          ;; a value, with zero or more metadata mods
          (=list (%maybe (?whitespace))
                 (=nested-metadata-sequence)
                 (?whitespace)
                 (%maybe (=nested-value))
                 (%any (=nested-metadata-mods)))
          ;; zero or more values, with metadata mods
          (=list (%maybe (?whitespace))
                 (=nested-metadata-sequence)
                 (?whitespace)
                 (%maybe (=nested-value))
                 (%some (=nested-metadata-mods)))
          ;; no atom value, zero or more metadata mods; the birthday trap
          (=list (%maybe (?whitespace))
                 (=nested-metadata-sequence)
                 (?whitespace)
                 (?satisfies (λ (_)
                               (declare (ignore _)))
                             (%any (=nested-value)))
                 (%any (=nested-metadata-mods))))
       (let* ((seq meta-sequence)
              (val meta-value)
              (mods (red-cat meta-mods))
              (list (list seq val mods))
              (value (denull list))
              (things (pad-things value))
              (val (red-cat things)))
         val)))))

(defmacro +nested-@-metadata ()
  "Define a variable capturing parser macro for @ with a single abutted metadata recall."
  `(=destructure
       (_ atom-sequence metadata-sequence _ _ _)
       (=list (?expression-starter)
              (=@-sequence)
              (=metadata-sequence)
              (%maybe (=hash))
              (%maybe (=comment))
              (?expression-terminator))
     (let* ((meta-seq (red-cat metadata-sequence))
            (atom-seq (make-seq atom-sequence))
            (value (list atom-seq meta-seq)))
       (list-string* value))))

(defmacro ~nested-@-metadata ()
  "Define a helper macro for handling abutted metadata in @ forms."
  `(if (equal name '=nested-@-form)
       '(+nested-@-metadata)
       '(?untrue)))

(defm def-nested-parser-form (name sequence value)
  "Define a macro for defining nested parsers."
  `(def-parser ,name ()
     (%or ,(~nested-@-metadata)
          (=destructure
              (,@(~mod _ _) atom-sequence _ atom-value atom-mods metadata hash _ _)
              (=list ,@(~mod (?blackspace) (?expression-starter))
                     ,sequence
                     (%maybe (?whitespace))
                     (%maybe ,value)
                     (%any (=nested-atom-mods))
                     (%maybe (=nested-metadata))
                     (%maybe (=nested-hash))
                     (%maybe (=nested-comment))
                     (?expression-terminator))
            (let* ((seq (make-seq atom-sequence))
                   (mods (red-cat (pad-things atom-mods)))
                   (meta (red-cat (pad-things metadata)))
                   (list (list seq atom-value mods meta hash))
                   (value (denull list))
                   (things (pad-things value)))
              (list-string* things))))))

(def-nested-parser-form =nested-@-form (=@-sequence) (=nested-@-value))
(def-nested-parser-form =nested-c-form (=c-sequence) (=nested-c-value))
(def-nested-parser-form =nested-grouping-form (=grouping-sequence) (=nested-grouping-value))
(def-nested-parser-form =nested-format-form (=format-sequence) (=nested-value))
(def-nested-parser-form =nested-datatype-form (=datatype-sequence) (=nested-value))

(def-parser =nested-atom-form ()
  "Match and return a nested atom."
  (%or (=nested-@-form)
       (=nested-c-form)
       (=nested-grouping-form)))

(def-parser =nested-expression ()
  "Match and return a nested MSL expression."
  (%or (=nested-@-form)
       (=nested-c-form)
       (=nested-grouping-form)
       (=nested-format-form)
       (=nested-datatype-form)))


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

(def parse-nested-msl (expr)
  "Parse an MSL expression."
  (parse expr (=nested-expression)))

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
