;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:nicknames #:s/core))

(in-package #:streams/core)

(defun mx-atom-name (mx-atom)
  "Return the name used to identify MX-ATOM."
  (streams/channels:name mx-atom))

(defun mx-atom-data (mx-atom)
  "Return the first value used to identify MX-ATOM."
  (streams/channels:data mx-atom))

(defmacro write-context (context)
  "Return a context symbol from CONTEXT."
  (let* ((symbol (ecase context
                  (m 'machine)
                  (w 'world)
                  (s 'stream)
                  (v 'view)
                  (c 'canon)))
         (var (mof:cat "STREAMS/ETHERS:*MX-" (string symbol) "*")))
    (read-from-string var)))

(defmacro build-context (context name)
  "Return a context instance from CONTEXT with NAME."
  (let* ((ctext (ecase context
                  (m "mx-machine")
                  (w "mx-world")
                  (s "mx-stream")
                  (v "mx-view")
                  (c "mx-canon")))
         (class-name (mof:cat "STREAMS/CHANNELS:" ctext)))
    (make-instance (read-from-string class-name) :name name)))

(defmacro context (context &body body)
  "Set the current context to CONTEXT then evaluate BODY."
  `(let ((streams/ethers:*context* (write-context ,context)))
     ,@body))

(defmacro m (&body body) `(context m ,@body))
(defmacro w (&body body) `(context w ,@body))
(defmacro s (&body body) `(context s ,@body))
(defmacro v (&body body) `(context v ,@body))
(defmacro c (&body body) `(context c ,@body))

(defmacro define-context-macros (contexts)
  "Define the macro shorthands for the context setters."
  `(progn
     ,@(loop :for c :in contexts
             :collect `(defmacro ,c (&body body)
                         (context ,c body)))))

(defun yield-context ()
  "Return the current context or the default context."
  (or streams/ethers:*context* streams/ethers:*mx-machine*))

(defun bracket-reader (stream char)
  "Use [/tmp/file.ext] as a shorthand for #P\"/tmp/file.ext\""
  (declare (ignore char))
  `(pathname ,@(read-delimited-list #\] stream t)))

(set-macro-character #\[ #'bracket-reader)
(set-macro-character #\] (get-macro-character #\) nil))

(defun valid-expr-p (expr)
  "Return true if EXPR is valid."
  (>= (length expr) 2))

(defun data-marker-p (value)
  "Return true if VALUE is a valid mx-atom data."
  (or (and (symbolp value) (not (keywordp value)))
      (stringp value)
      (numberp value)
      (consp value)))

(defun metadata-marker-p (value)
  "Return true if VALUE is a valid mx-atom metadata."
  (not (data-marker-p value)))

(defun bounds (raw-expr)
  "Return the indices for the start and end of immediate valid data of RAW-EXPR. If none are found, return NIL."
  (let ((length (length raw-expr)))
    (when (member-if #'data-marker-p raw-expr)
      (destructuring-bind (head &body body)
          raw-expr
        (let* ((start (if (data-marker-p head)
                          0
                          (1+ (position-if #'data-marker-p body))))
               (end (when (numberp start)
                      (if (member-if #'metadata-marker-p (subseq raw-expr start))
                          (+ start (1- (position-if #'metadata-marker-p (nthcdr start raw-expr))))
                          (1- length)))))
          (values start end))))))

(defun split-symbol (symbol)
  "Return a new list containing the prefix and the body of symbol from SYMBOL."
  (let* ((string (streams/common:string-convert symbol))
         (length (length string)))
    (declare (ignorable length))
    (if (> length 1)
        (list (intern (streams/common:string-convert (elt string 0)))
              (read-from-string (subseq string 1)))
        (list symbol))))

(defun prefixedp (symbol prefix)
  "Return true if SYMBOL contains the prefix PREFIX."
  (and (symbolp symbol)
       (let* ((string (streams/common:string-convert symbol))
              (length (length string)))
         (and (> length 1)
              (char= prefix (elt string 0))))))

(defun @-prefixed-p (symbol)
  "Return true if SYMBOL is prefixed with the @ identifier."
  (prefixedp symbol #\@))

(defun split-prefixed-items (list)
  "Return a new list with split items that are prefixed."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((consp (car args)) (fn (cdr args) (cons (fn (car args) nil) acc)))
                   ((@-prefixed-p (car args))
                    (fn (cdr args) (append acc (nreverse (split-symbol (car args))))))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (fn list nil)))

(defun read-expr-prime (raw-expr)
  "Return RAW-EXPR as valid lisp expression."
  (let ((value (streams/common:read-string-with-preserved-case raw-expr)))
    (when (valid-expr-p value)
      (split-prefixed-items value))))

(defun examine-expr (raw-expr)
  "Print information about RAW-EXPR."
  (loop :for e :in (read-expr-prime raw-expr) :do (format t "~S~20T~S~%" e (type-of e))))

(defun primary-values (expr)
  "Return the primary values from EXPR; return NIL if none are found."
  (destructuring-bind (category name &body body)
      expr
    (declare (ignore category name))
    (when (data-marker-p (first body))
      (multiple-value-bind (start end)
          (bounds body)
        (subseq body start (1+ end))))))

(defun secondary-values (expr)
  "Return the secondary values—metadata, etc—from EXPR; return NIL if none are found."
  (destructuring-bind (category name &body body)
      expr
    (declare (ignore category name))
    (let ((index (position-if #'metadata-marker-p body)))
      (when index
        (subseq body index)))))

(defun read-expr (expr)
  "Read an EXPR as a string and return an object that contains the parsed information."
  (let ((expr (read-expr-prime expr)))
    (destructuring-bind (category name &body body)
        expr
      (labels ((fn (args data metadata)
                 (cond ((null args) (streams/channels:make-mx-atom
                                     category name data (nreverse metadata)))
                       (t (multiple-value-bind (start end)
                              (bounds args)
                            (fn (nthcdr (1+ end) args)
                                data
                                (acons (car args) (subseq args start (1+ end)) metadata)))))))
        (fn (secondary-values body) (primary-values expr) nil)))))

(defun resolve-atom (atom)
  "Expand the values inside ATOM then assign them to the corresponding stores."
  nil)

