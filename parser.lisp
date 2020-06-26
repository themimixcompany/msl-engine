;;;; parser.lisp

(uiop:define-package #:streams/parser
  (:use #:cl
        #:streams/common
        #:maxpc)
  (:export #:parse-msl
           #:parse-setters
           #:parse-explain))

(in-package #:streams/parser)


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

(defun diag (&optional message value)
  "Print a diagnostic message."
  (format t "~%~5T>>> ~A ~A~%~%" message value))


;; MSL Explainer System (MSLES)

(define-parser =msl-expression ()
  "Match and return an MSL expression."
  (%or '@-form
       'grouping-form
       'canon-form
       'prelude-form
       'datatype-form
       'format-form
       'regex-selector))
;;

(defun parse-msl (expr)
  "Parse an MSL expression."
  (parse expr (=msl-expression)))
;;

(defun parse-explain (expr)
  "Parse and explain an MSL expression."
  (let ((parsed-atom (parse-msl expr))
        (atom-explainer '(atom-seq atom-value atom-mods metadata hash comment)))
    (format t "~%")
    (princ "  MX OUTPUT ")
    (format t "~%")
    (format t "~%~S~%" parsed-atom)
    (format t "~%  ~A~13T| ~15T ~A" "LABEL" "MX OUTPUT")
    (format t "~%")
    (explain (collate atom-explainer parsed-atom))
    (format t "~%~%")
    (princ "  MSL ")
    (format t "~%~%")
    expr))

;;

(defun parse-setters (expr)
  "Parse an MSL expression and explain as MIL single-setters."
  (format t "~%")
  (let ((parsed-atom (parse-msl expr))
        (atom-explainer '(atom-seq atom-value atom-mods metadata hash comment)))
    (when (explain-lines parsed-atom) parsed-atom)))


(defun explain-lines (setters &optional (line-num 1))
  "Print each setter from a list on a separate line."
  (cond ((not setters) NIL)
        (t (format t "~A.~4T~S~%" line-num (car setters)) (explain-lines (cdr setters) (+ line-num 1)) t)))


(defun explain (item-list)
  "Show a printed explainer for a parsed MSL expression."
  (let* ((item (car item-list))
         (item-label (car item))
         (item-value (car (cdr item))))
    (cond ((not item-list) nil)
          (t (format t "~%  ~A~13T| ~15T ~S" item-label item-value) (explain (cdr item-list))))))
;;

(defun collate (&rest lists)
  "Combine the first item of each list, second item, etc."
  (apply #'mapcar #'list lists))
;;

;; STREAM (CHARACTER) PARSERS

;; TEST PARSERS (Don't return a value)

(define-parser ?whitespace ()
  "Match one or more whitespace characters."
  (?seq (%some (maxpc.char:?whitespace))))
;;

(define-parser ?hexp ()
  "Match a single hex character."
  (?satisfies 'hex-char-p))
;;

(define-parser ?value-terminator ()
  "Match the end of a value."
  (%or
   'nested-atom
   'metadata-sequence
   'regex-selector
   'bracketed-transform-selector
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

(define-parser ?expression-terminator ()
  "Match the end of an expression."
  (?seq
   (?eq #\right_parenthesis)))
;;

;; SINGLE VALUE PARSERS (Return one value.)

(define-parser =sha256 ()
  "Match and return a SHA-256 string."
  (=subseq (?seq (?satisfies 'length-64-p
                             (=subseq (%some 'hexp))))))
;;

(define-parser =msl-key ()
  "Match and return a valid MSL key."
  (=subseq (?seq (%some (?satisfies 'alphanumericp))
                 (%any (?seq (%maybe (?eq #\-))
                             (%some (?satisfies 'alphanumericp)))))))
;;

;; Dummy Filespec
(define-parser =msl-filespec ()
  "Match and return a URI filespec or URL."
  (=subseq (%some (?satisfies 'alphanumericp))))

(define-parser =msl-hash ()
  "Match and return a hash value."
  (=destructure (_ ns hash)
                (=list 'whitespace
                       (=subseq (?eq #\#))
                       (=sha256))
    (list (list ns) (list hash))))
;;

(define-parser =msl-value ()
  "Match and return a raw value."
  (%and
   (?not 'value-terminator)
   (=destructure (_ value)
                 (=list
                  'whitespace
                  (=subseq (%some (?not 'value-terminator)))))))
;;

(define-parser =msl-comment ()
  "Match a comment."
  (=destructure (_ _ comment)
                (=list 'whitespace
                       (maxpc.char:?string "//")
                       (=subseq (%some (?not (%or 'expression-terminator)))))))
;;

;; Nested Expression Parsers

(define-parser =nested-@ ()
  "Match and return a nested atom."
  (=destructure (_ atom)
                (=list 'whitespace
                       '@-form)))
;;

(define-parser =nested-atom ()
  "Match and return a nested atom."
  (=destructure (_ atom)
                (=list 'whitespace
                       (%or '@-form
                            'canon-form
                            'grouping-form))))
;;

(define-parser =nested-group ()
  "Match and return a nested atom."
  (=destructure (_ atom)
                (=list 'whitespace
                       'grouping-form)))
;;

(define-parser =nested-canon ()
  "Match and return a nested atom."
  (=destructure (_ atom)
                (=list 'whitespace
                       'canon-form)))
;;

;; Namespace Parsers

(define-parser =@-namespace ()
  "Match and return the @ namespace."
  (=subseq (?eq #\@)))
;;

(define-parser =atom-namespace ()
  "Match and return an atom namespace."
  (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v #\c #\@))))))
;;

(define-parser =grouping-namespace ()
  "Match and return m w s v namespace."
  (=subseq (?satisfies (lambda (c) (member c '(#\m #\w #\s #\v))))))
;;

(define-parser =regex-namespace ()
  "Match and return the / namespace."
  (=subseq (?eq #\/)))
;;

(define-parser =prelude-namespace ()
  "Match and return the msl namespace."
  (=subseq (maxpc.char:?string "msl")))
;;

(define-parser =metadata-namespace ()
  "Match and return the : namespace."
  (=subseq (?eq #\:)))
;;

(define-parser =canon-namespace ()
  "Match and return the c namespace."
  (=subseq (?eq #\c)))
;;

(define-parser =datatype-namespace ()
  "Match and return the d namespace."
  (=subseq (?eq #\d)))
;;

(define-parser =format-namespace ()
  "Match and return the f namespace."
  (=subseq (?eq #\f)))
;;

;; LIST OF VALUES PARSERS (Return a list.)

;; Value Types

(define-parser =@-value ()
  "Match and return a valid value for @."
  (%or 'nested-@
       'nested-group
       'msl-value))
;;

(define-parser =c-value ()
  "Match and return a valid value for c."
  (%or 'nested-@
       'nested-canon
       'msl-value))
;;

(define-parser =group-value ()
  "Match and return a valid value for m w s v."
  (%or 'nested-@
       'nested-group
       'nested-canon
       'msl-value))


;; Namespace Sequences

(define-parser =@-sequence ()
  "Match and return the key sequence for an @."
  (=list (=destructure (ns _)
                       (=list (=@-namespace)
                              (%maybe 'whitespace)))
         'msl-key))
;;

(define-parser =grouping-sequence ()
  "Match and return the key sequence for an atom."
  (=list (=destructure (ns _)
                       (=list (=grouping-namespace)
                              'whitespace))
         'msl-key))
;;

(define-parser =canon-sequence ()
  "Match and return the key sequence for canon."
  (=list (=destructure (ns _)
                       (=list (=canon-namespace)
                              'whitespace))
         'msl-key))
;;

(define-parser =prelude-sequence ()
  "Match and return the key sequence for a prelude."
  (=list (=destructure (ns _)
                       (=list (=prelude-namespace)
                              'whitespace))
         'msl-key))
;;

(define-parser =metadata-sequence ()
  "Match and return key sequence for : namespace."
  (=destructure (_ ns key)
                (=list 'whitespace
                       (=metadata-namespace)
                       'msl-key)
    (list ns key)))
;;

(define-parser =regex-selector ()
  "Match and return the key sequence for /."
  (=destructure (regex-list)
                (=list
                 (%some
                  (=destructure (_ _ regex _ env value)
                                (=list 'whitespace
                                       (=regex-namespace)
                                       (=subseq (%some (?satisfies 'alphanumericp)))
                                       (=regex-namespace)
                                       (%maybe (=subseq (%some (?satisfies 'alphanumericp))))
                                       (%maybe 'msl-value))
                    (list regex env value))))
    (cond (regex-list (list (list "/") regex-list NIL NIL NIL NIL)))))

;;

(define-parser =bracketed-transform-selector ()
  "Match and return the key sequence for []."
  (=destructure (transform-list)
                (=list
                 (%some
                  (=destructure (_ _ url _)
                                (=list 'whitespace
                                       (?eq #\[)
                                       (=msl-filespec)
                                       (?eq #\])))))

    (cond (transform-list (list (list "[]") transform-list NIL NIL NIL NIL)))))
;;

(define-parser =datatype-sequence ()
  "Match and return key sequence for d."
  (=destructure (atom _ key)
                (=list (=datatype-namespace)
                       'whitespace
                       'msl-key)
    (list atom key)))
;;

(define-parser =format-sequence ()
  "Match and return key sequence for f."
  (=destructure (atom _ key)
                (=list (=format-namespace)
                       'whitespace
                       'msl-key)
    (list atom key)))
;;


;; Mods

(define-parser =atom-mods ()
  "Match and return key sequence for / [] d f namespace."
  (%or 'regex-selector
       'datatype-form
       'format-form
       'bracketed-transform-selector))
;;

(define-parser =format-mods ()
  "Match and return key sequence for / d f namespace."
  (%or 'regex-selector
       'datatype-form
       'format-form))

;;

(define-parser =datatype-mods ()
  "Match and return key sequence for / namespace."
  (%or 'regex-selector))
;;


;; Atom Forms (Single-Setter)

(define-parser =@-form ()
  "Match and return an atom in the @ namespace."
  (let ((atom-val) (atom-seq) (meta-seq))
    (=destructure (_ atom-seq atom-value atom-mods metadata hash _ _)
                  (=list (?eq #\left_parenthesis)
                         (=transform
                          (=@-sequence)
                          (lambda (seq)
                            (setf atom-seq seq)))
                         (=transform (%any '@-value)
                                     (lambda (val)
                                       (cond (val (setf atom-val val))
                                             (t (setf atom-val NIL)))))
                         (%any (=destructure (mod-seq &optional mod-value mod-mods mod-meta mod-hash mod-comment)
                                             'atom-mods
                                 (list (append atom-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))
                         (%maybe (%or
                                  ;; one or more metadata keys... each one having:
                                  (%some (=destructure (meta-seq meta-value meta-mods)
                                                       (%or
                                                        ;; a value, maybe mods (META 1, the "value" case.)
                                                        ;; %some value + %any mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 1" seq)
                                                                  (setf meta-seq seq)))
                                                               (%some '@-value)
                                                               (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                   'atom-mods
                                                                       (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                                        ;; no value, with mods (META 2, the "no value" case.)
                                                        ;; %any value + %some mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 2" seq)
                                                                  (setf meta-seq seq)))
                                                               (%any '@-value)
                                                               (%some (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                    'atom-mods
                                                                        (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))))
                                           (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))

                                  ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
                                  ;; %maybe value (if no atom-val) + %any mods
                                  (=destructure (meta-seq meta-value meta-mods)
                                                (=list (=transform
                                                        'metadata-sequence
                                                        (lambda (seq)
                                                          ;;(diag "META 3" seq)
                                                          (setf meta-seq seq)))
                                                       (?satisfies (lambda (val)
                                                                     (declare (ignore val)) (unless atom-val t))
                                                                   (%any '@-value))
                                                       (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                           'atom-mods
                                                               (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                    (list (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))))
                         (%maybe (=destructure (hash-seq hash-value)
                                               'msl-hash
                                   (list (list (append atom-seq hash-seq) hash-value))))
                         (%maybe 'msl-comment)
                         'expression-terminator)
      (append (append-each (append (list (list atom-seq atom-value)) atom-mods) metadata) hash))))
;;;;

(define-parser =canon-form ()
  "Match and return an atom in the c namespace."
  (let ((atom-val) (atom-seq) (meta-seq))
    (=destructure (_ atom-seq atom-value atom-mods metadata hash _ _)
                  (=list (?eq #\left_parenthesis)
                         (=transform
                          (=canon-sequence)
                          (lambda (seq)
                            (setf atom-seq seq)))
                         (=transform (%any 'c-value)
                                     (lambda (val)
                                       (cond (val (setf atom-val val))
                                             (t (setf atom-val NIL)))))
                         (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                             'atom-mods
                                 (list (append atom-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))
                         (%maybe (%or
                                  ;; one or more metadata keys... each one having:
                                  (%some (=destructure (meta-seq meta-value meta-mods)
                                                       (%or
                                                        ;; a value, maybe mods (META 1, the "value" case.)
                                                        ;; %some value + %any mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 1" seq)
                                                                  (setf meta-seq seq)))
                                                               (%some 'c-value)
                                                               (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                   'atom-mods
                                                                       (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                                        ;; no value, with mods (META 2, the "no value" case.)
                                                        ;; %any value + %some mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 2" seq)
                                                                  (setf meta-seq seq)))
                                                               (%any 'c-value)
                                                               (%some (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                    'atom-mods
                                                                        (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))))
                                           (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))

                                  ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
                                  ;; %maybe value (if no atom-val) + %any mods
                                  (=destructure (meta-seq meta-value meta-mods)
                                                (=list (=transform
                                                        'metadata-sequence
                                                        (lambda (seq)
                                                          ;;(diag "META 3" seq)
                                                          (setf meta-seq seq)))
                                                       (?satisfies (lambda (val)
                                                                     (declare (ignore val)) (unless atom-val t))
                                                                   (%any 'c-value))
                                                       (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                           'atom-mods
                                                               (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                    (list (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))))
                         (%maybe (=destructure (hash-seq hash-value)
                                               'msl-hash
                                   (list (list (append atom-seq hash-seq) hash-value))))
                         (%maybe 'msl-comment)
                         'expression-terminator)
      (append (append-each (append (list (list atom-seq atom-value)) atom-mods) metadata) hash))))
;;;;

(define-parser =grouping-form ()
  "Match and return an atom in the m w s v namespace."
  (let ((atom-val) (atom-seq) (meta-seq))
    (=destructure (_ atom-seq atom-value atom-mods metadata hash _ _)
                  (=list (?eq #\left_parenthesis)
                         (=transform
                          (=grouping-sequence)
                          (lambda (seq)
                            (setf atom-seq seq)))
                         (=transform (%any 'group-value)
                                     (lambda (val)
                                       (cond (val (setf atom-val val))
                                             (t (setf atom-val NIL)))))
                         (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                             'atom-mods
                                 (list (append atom-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))
                         (%maybe (%or
                                  ;; one or more metadata keys... each one having:
                                  (%some (=destructure (meta-seq meta-value meta-mods)
                                                       (%or
                                                        ;; a value, maybe mods (META 1, the "value" case.)
                                                        ;; %some value + %any mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 1" seq)
                                                                  (setf meta-seq seq)))
                                                               (%some 'group-value)
                                                               (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                   'atom-mods
                                                                       (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                                        ;; no value, with mods (META 2, the "no value" case.)
                                                        ;; %any value + %some mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 2" seq)
                                                                  (setf meta-seq seq)))
                                                               (%any 'group-value)
                                                               (%some (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                    'atom-mods
                                                                        (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))))
                                           (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))

                                  ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
                                  ;; %maybe value (if no atom-val) + %any mods
                                  (=destructure (meta-seq meta-value meta-mods)
                                                (=list (=transform
                                                        'metadata-sequence
                                                        (lambda (seq)
                                                          ;;(diag "META 3" seq)
                                                          (setf meta-seq seq)))
                                                       (?satisfies (lambda (val)
                                                                     (declare (ignore val)) (unless atom-val t))
                                                                   (%any 'group-value))
                                                       (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                           'atom-mods
                                                               (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                    (list (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))))
                         (%maybe (=destructure (hash-seq hash-value)
                                               'msl-hash
                                   (list (list (append atom-seq hash-seq) hash-value))))
                         (%maybe 'msl-comment)
                         'expression-terminator)
      (append (append-each (append (list (list atom-seq atom-value)) atom-mods) metadata) hash))))
;;;;

(define-parser =format-form ()
   "Match and return an atom in the f namespace."
   (let ((saved-val))
     (=destructure (_ _ atom-seq atom-value atom-mods metadata comment _)
                   (=list 'whitespace
                          (?eq #\left_parenthesis)
                          (=format-sequence)
                          (=transform (%any 'msl-value)
                                      (lambda (val)
                                              (cond (val (setf saved-val val))
                                                    (t (setf saved-val NIL)))))
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
                                               (?satisfies (lambda (val)
                                                                   (declare (ignore val)) (unless saved-val t))
                                                           (%maybe 'msl-value))
                                               (%any 'format-mods))
                                        (list meta-seq meta-value meta-mods))))
                          (%maybe 'msl-comment)
                          'expression-terminator)
                   (list atom-seq atom-value atom-mods metadata NIL comment))))
;;


; (define-parser =format-form ()
;   "Match and return an atom in the f namespace."
;   (let ((atom-val) (atom-seq) (meta-seq))
;     (=destructure (_ _ atom-sequence atom-value atom-mods metadata _ _)
;                   (=list 'whitespace
;                          (?eq #\left_parenthesis)
;                          (=transform
;                           'format-sequence
;                           (lambda (seq)
;                             ;;(diag "ATOM-SEQ" atom-seq)
;                             ;;(diag "SEQ" seq)
;                             (setf atom-seq seq)))
;                          (=transform (%any 'msl-value)
;                                      (lambda (val)
;                                        (cond (val (setf atom-val val))
;                                              (t (setf atom-val NIL)))))
;                          (%any (=destructure (mod-seq &optional mod-value mod-mods mod-meta mod-hash mod-comment)
;                                              (=transform 'format-form
;                                                          (lambda (val)
;                                                            ;;(diag "ATOM MODS" val)
;                                                            val))
;                                  (list (append atom-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))
;                          (%maybe (%or
;                                   ;; one or more metadata keys... each one having:
;                                   (%some (=destructure (meta-seq meta-value meta-mods)
;                                                        (%or
;                                                         ;; a value, maybe mods (META 1, the "value" case.)
;                                                         ;; %some value + %any mods
;                                                         (=list (=transform
;                                                                 'metadata-sequence
;                                                                 (lambda (seq)
;                                                                   ;;(diag "META 1" seq)
;                                                                   (setf meta-seq seq)))
;                                                                (%some 'msl-value)
;                                                                (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
;                                                                                    (=transform 'format-mods
;                                                                                                (lambda (val)
;                                                                                                  ;;(diag "META 1 MODS")
;                                                                                                  val))
;                                                                        (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
;                                                         ;; no value, with mods (META 2, the "no value" case.)
;                                                         ;; %any value + %some mods
;                                                         (=list (=transform
;                                                                 'metadata-sequence
;                                                                 (lambda (seq)
;                                                                   ;;(diag "META 2" seq)
;                                                                   (setf meta-seq seq)))
;                                                                (%any 'msl-value)
;                                                                (%some (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
;                                                                                     (=transform 'format-mods
;                                                                                                 (lambda (val)
;                                                                                                   ;;(diag "META 2 MODS")
;                                                                                                   val))
;                                                                         (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))))
;                                            (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))
;
;                                   ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
;                                   ;; %maybe value (if no atom-val) + %any mods
;                                   (=destructure (meta-seq meta-value meta-mods)
;                                                 (=list (=transform
;                                                         'metadata-sequence
;                                                         (lambda (seq)
;                                                           ;;(diag "META 3" seq)
;                                                           (setf meta-seq seq)))
;                                                        (?satisfies (lambda (val)
;                                                                      (declare (ignore val)) (unless atom-val t))
;                                                                    (%any 'msl-value))
;                                                        (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
;                                                                            (=transform 'format-mods
;                                                                                        (lambda (val)
;                                                                                          ;;(diag "META 3 MODS")
;                                                                                          val))
;                                                                (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
;                                     (list (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))))
;                          (%maybe 'msl-comment)
;                          'expression-terminator)
;       (append (append-each (append (list (list atom-sequence atom-value)) atom-mods) metadata) NIL))))
; ;;;;

; (define-parser =datatype-form ()
;   "Match and return an atom in the d namespace."
;   (let ((atom-val) (atom-seq) (meta-seq))
;     (=destructure (_ _ atom-seq atom-value atom-mods metadata _ _)
;                   (=list 'whitespace
;                          (?eq #\left_parenthesis)
;                          (=transform
;                           (=datatype-sequence)
;                           (lambda (seq)
;                             (setf atom-seq seq)))
;                          (=transform (%any 'msl-value)
;                                      (lambda (val)
;                                        (cond (val (setf atom-val val))
;                                              (t (setf atom-val NIL)))))
;                          (%any (=destructure (mod-seq &optional mod-value mod-mods mod-meta mod-hash mod-comment)
;                                              'format-mods
;                                  (list (append atom-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))
;                          (%maybe (%or
;                                   ;; one or more metadata keys... each one having:
;                                   (%some (=destructure (meta-seq meta-value meta-mods)
;                                                        (%or
;                                                         ;; a value, maybe mods (META 1, the "value" case.)
;                                                         ;; %some value + %any mods
;                                                         (=list (=transform
;                                                                 'metadata-sequence
;                                                                 (lambda (seq)
;                                                                   ;;(diag "META 1" seq)
;                                                                   (setf meta-seq seq)))
;                                                                (%some 'msl-value)
;                                                                (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
;                                                                                    'format-mods
;                                                                        (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
;                                                         ;; no value, with mods (META 2, the "no value" case.)
;                                                         ;; %any value + %some mods
;                                                         (=list (=transform
;                                                                 'metadata-sequence
;                                                                 (lambda (seq)
;                                                                   ;;(diag "META 2" seq)
;                                                                   (setf meta-seq seq)))
;                                                                (%any 'msl-value)
;                                                                (%some (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
;                                                                                     'format-mods
;                                                                         (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))))
;                                            (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))
;
;                                   ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
;                                   ;; %maybe value (if no atom-val) + %any mods
;                                   (=destructure (meta-seq meta-value meta-mods)
;                                                 (=list (=transform
;                                                         'metadata-sequence
;                                                         (lambda (seq)
;                                                           ;;(diag "META 3" seq)
;                                                           (setf meta-seq seq)))
;                                                        (?satisfies (lambda (val)
;                                                                      (declare (ignore val)) (unless atom-val t))
;                                                                    (%any 'msl-value))
;                                                        (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
;                                                                            'format-mods
;                                                                (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
;                                     (list (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))))
;                          (%maybe 'msl-comment)
;                          'expression-terminator)
;       (append (append-each (append (list (list atom-seq atom-value)) atom-mods) metadata) NIL))))
; ;;;;

(define-parser =datatype-form ()
   "Match and return an atom in the d namespace."
   (let ((saved-val))
        (=destructure (_ _ atom-seq atom-value atom-mods metadata comment _)
                      (=list 'whitespace
                             (?eq #\left_parenthesis)
                             (=datatype-sequence)
                             (=transform (%any 'msl-value)
                                         (lambda (val)
                                                 (cond (val (setf saved-val val))
                                                       (t (setf saved-val NIL)))))
                             (%maybe 'datatype-mods)
                             (%maybe (%or
                                         (%some (=destructure (meta-seq meta-value meta-mods)
                                                 (%or
                                                   (=list 'metadata-sequence
                                                          'msl-value
                                                          (%any 'datatype-mods))
                                                   (=list 'metadata-sequence
                                                          (%maybe 'msl-value)
                                                          (%some 'datatype-mods)))
                                                 (list meta-seq meta-value meta-mods)))
                                         (=destructure (meta-seq meta-value meta-mods)
                                           (=list 'metadata-sequence
                                                  (?satisfies (lambda (val)
                                                                      (declare (ignore val)) (unless saved-val t))
                                                              (%maybe 'msl-value))
                                                  (%any 'datatype-mods))
                                          (list meta-seq meta-value meta-mods))))
                             (%maybe 'msl-comment)
                             'expression-terminator)
                      (list atom-seq atom-value atom-mods metadata NIL comment))))
;;



(define-parser =prelude-form ()
  "Match and return an atom in the msl namespace."
  (let ((atom-val) (atom-seq) (meta-seq))
    (=destructure (_ atom-seq atom-value atom-mods metadata hash _ _)
                  (=list (?eq #\left_parenthesis)
                         (=transform
                          (=prelude-sequence)
                          (lambda (seq)
                            (setf atom-seq seq)))
                         (=transform (%any 'msl-value)
                                     (lambda (val)
                                       (cond (val (setf atom-val val))
                                             (t (setf atom-val NIL)))))
                         (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                             'atom-mods
                                 (list (append atom-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))
                         (%maybe (%or
                                  ;; one or more metadata keys... each one having:
                                  (%some (=destructure (meta-seq meta-value meta-mods)
                                                       (%or
                                                        ;; a value, maybe mods (META 1, the "value" case.)
                                                        ;; %some value + %any mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 1" seq)
                                                                  (setf meta-seq seq)))
                                                               (%some 'msl-value)
                                                               (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                   'atom-mods
                                                                       (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                                        ;; no value, with mods (META 2, the "no value" case.)
                                                        ;; %any value + %some mods
                                                        (=list (=transform
                                                                'metadata-sequence
                                                                (lambda (seq)
                                                                  ;;(diag "META 2" seq)
                                                                  (setf meta-seq seq)))
                                                               (%any 'msl-value)
                                                               (%some (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                                    'atom-mods
                                                                        (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment)))))
                                           (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))

                                  ;; single metadata key, no value, maybe mods (META 3, the ":birthday trap.")
                                  ;; %maybe value (if no atom-val) + %any mods
                                  (=destructure (meta-seq meta-value meta-mods)
                                                (=list (=transform
                                                        'metadata-sequence
                                                        (lambda (seq)
                                                          ;;(diag "META 3" seq)
                                                          (setf meta-seq seq)))
                                                       (?satisfies (lambda (val)
                                                                     (declare (ignore val)) (unless atom-val t))
                                                                   (%any 'msl-value))
                                                       (%any (=destructure (mod-seq mod-value mod-mods mod-meta mod-hash mod-comment)
                                                                           'atom-mods
                                                               (list (append atom-seq meta-seq mod-seq) mod-value mod-mods mod-meta mod-hash mod-comment))))
                                    (list (cons (list (append atom-seq meta-seq) meta-value) meta-mods)))))
                         (%maybe (=destructure (hash-seq hash-value)
                                               'msl-hash
                                   (list (list (append atom-seq hash-seq) hash-value))))
                         (%maybe 'msl-comment)
                         'expression-terminator)
      (append (append-each (append (list (list atom-seq atom-value)) atom-mods) metadata) hash))))
;;;;

(defun append-each (base-list item-list)
  "Add each item *on each item* of item-list to base-list."
  (cond ((null item-list) base-list)
        (t (append-each (append base-list (car item-list)) (cdr item-list)))))
