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

(defmacro write-namespace (namespace)
  "Return a namespace symbol from NAMESPACE."
  (let* ((symbol (ecase namespace
                   (m 'machine)
                   (w 'world)
                   (s 'stream)
                   (v 'view)
                   (c 'canon)))
         (var (mof:cat "STREAMS/ETHERS:*MX-" (string symbol) "*")))
    (read-from-string var)))

(defmacro build-namespace (namespace name)
  "Return a namespace instance from NAMESPACE with NAME."
  (let* ((ctext (ecase namespace
                  (m "mx-machine")
                  (w "mx-world")
                  (s "mx-stream")
                  (v "mx-view")
                  (c "mx-canon")))
         (class-name (mof:cat "STREAMS/CHANNELS:" ctext)))
    (make-instance (read-from-string class-name) :name name)))

(defmacro namespace (namespace &body body)
  "Set the current namespace to NAMESPACE then evaluate BODY. Restore the namespace after the evaluation of BODY."
  `(let ((current-namespace streams/ethers:*namespace*)
         (streams/ethers:*namespace* (write-namespace ,namespace)))
     ,@body
     (setf streams/ethers:*namespace* current-namespace)))

(defmacro m (&body body) `(namespace m ,@body))
(defmacro w (&body body) `(namespace w ,@body))
(defmacro s (&body body) `(namespace s ,@body))
(defmacro v (&body body) `(namespace v ,@body))
(defmacro c (&body body) `(namespace c ,@body))
;;(defmacro @ (&body body) `(namespace c ,@body))

(defmacro define-namespace-macros (namespaces)
  "Define the macro shorthands for the namespace setters."
  `(progn
     ,@(loop :for c :in namespaces
             :collect `(defmacro ,c (&body body)
                         (namespace ,c body)))))

(defun yield-namespace ()
  "Return the current namespace or the default namespace."
  (or streams/ethers:*namespace* streams/ethers:*mx-machine*))

(defun valid-expr-p (expr)
  "Return true if EXPR is valid."
  (>= (length expr) 1))

(defun data-marker-p (value)
  "Return true if VALUE is a valid mx-atom data."
  (or (and (symbolp value) (not (keywordp value)))
      (stringp value)
      (numberp value)
      (consp value)
      (pathnamep value)))

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

(defun split-prefixes (list)
  "Return a new list where the first item is split if prefixed; also apply to sublists that are prefixed."
  (labels ((fn (args acc &optional flag)
             (cond ((null args) (nreverse acc))
                   ((and flag (@-prefixed-p (car args)))
                    (fn (cdr args) (nconc (nreverse (split-symbol (car args))) acc) nil))
                   ((consp (car args))
                    (fn (cdr args) (cons (fn (car args) nil t) acc)))
                   (t (fn (cdr args) (cons (car args) acc) nil)))))
    (fn list nil t)))

(defun primary-values (expr)
  "Return the primary values from EXPR; return NIL if none are found."
  (destructuring-bind (_ __ &optional &body body)
      expr
    (declare (ignore _ __))
    (when (and body (data-marker-p (first body)))
      (multiple-value-bind (start end)
          (bounds body)
        (subseq body start (1+ end))))))

(defun secondary-values (expr)
  "Return the secondary values—metadata, etc—from EXPR; return NIL if none are found."
  (destructuring-bind (_ __ &body body)
      expr
    (declare (ignore _ __))
    (let ((index (position-if #'metadata-marker-p body)))
      (when index
        (subseq body index)))))

(defun valid-id-p (key)
  "Return true if KEY is a valid identifier for mx-atoms."
  (when (cl-ppcre:scan "^([a-zA-Z]+)(-?[a-zA-Z0-9])*$" key)
    t))

(defun valid-key-p (key)
  "Return true if NAME is a valid key for an mx-atom."
  (let ((val (s/common:string-convert key)))
    (when (valid-id-p val)
      t)))

(defun key (value)
  "Extract the key used in VALUE."
  (destructuring-bind (ns key &optional &body body)
      value
    (declare (ignore ns body))
    key))

(defun valid-keys-p (value)
  "Return true if the keys in VALUE are valid."
  (labels ((fn (args)
             (cond ((null args) t)
                   ((consp (car args)) (fn (car args)))
                   ((unless (valid-key-p (car args))) nil)
                   (t (fn (cdr args))))))
    (and (valid-key-p (key value))
         (fn value))))

(defun valid-form-p (value)
  "Return true if EXPR is a valid mx-atom expression."
  ;; The first pass goes through all the items and checks for the keys
  (and (valid-keys-p value)))

(defun read-expr (expr)
  "Read EXPR and break it down into multiple values."
  (let ((expr (split-prefixes expr)))
    (destructuring-bind (ns name &optional &rest _)
        expr
      (declare (ignore _))
      (labels ((fn (args data metadata)
                 (cond ((null args) (values ns name data (nreverse metadata)))
                       (t (multiple-value-bind (start end)
                              (bounds args)
                            (fn (nthcdr (1+ end) args)
                                data
                                (acons (car args) (subseq args start (1+ end)) metadata)))))))
        (fn (secondary-values expr) (primary-values expr) nil)))))

(defun read-expr-from-string (raw-expr)
  "Read an EXPR as a string and return values that represent the parsed information."
  (let ((value (streams/common:read-string-with-preserved-case raw-expr)))
    (when (valid-expr-p value)
      (let ((value (split-prefixes value)))
        (when (valid-form-p value)
          (read-expr value))))))

(defun examine-expr (raw-expr)
  "Print information about RAW-EXPR."
  (loop :for e :in (read-expr-from-string raw-expr) :do (format t "~S~20T~S~%" e (type-of e))))

(defun resolve-atom (atom)
  "Expand the values inside ATOM then assign them to the corresponding stores."
  (declare (ignore atom))
  nil)

(defun build-pairs (items)
  "Group items into pairs."
  (when (evenp (length items))
    (labels ((fn (items acc)
               (cond ((null items) (nreverse acc))
                     (t (fn (cddr items)
                            (cons (list (first items) (second items))
                                  acc))))))
      (fn items nil))))

(defun build-map (items &key (test #'keywordp) (constructor #'cons))
  "Create key-value mappings from ITEMS."
  (loop :for item :in (build-pairs items)
        :when (funcall test (car item))
        :collect (destructuring-bind (k v)
                     item
                   (funcall constructor k v))))

;;; Should expansion should here, before the values gets stored?
(defun build-mx-atom (expr)
  "Return an mx-atom instance from EXPR."
  (multiple-value-bind (ns name data metadata)
      (read-expr expr)
    (streams/channels:make-mx-atom ns name data metadata)))

(defmacro with-current-namespace (&body body)
  "Evaluate BODY in the current namespace."
  `(let* ((namespace (yield-namespace))
          (table (streams/channels:table (yield-namespace))))
     ,@body))

(defun recall-value (key)
  "Return a value in NSPACE indicated by HASH."
  (with-current-namespace
    (multiple-value-bind (v presentp)
        (gethash key table)
      (when presentp
        (values v table namespace)))))

(defun update-colon-selector (items key value)
  "Update a specific colon selector under KEY with VALUE within ITEMS."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((eql (caar args) key) (fn (cdr args) (acons key value acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (fn items nil)))

;;; Implement selective updates
(defun update-value (key value)
  "Store or update a value in NSPACE indicated by HASH with VALUE."
  (with-current-namespace
    (setf (gethash key table) value)
    (values value table namespace)))

(defun eval-expr (expr)
  "Evaluate EXPR, store the result into the current nspace, then return the mx-atom instance and the nspace as multiple values."
  (let ((expr (split-prefixes expr)))
    (destructuring-bind (ns name &optional &body body)
          expr
        ;; What is the role of NS?
        (declare (ignorable ns))
        (if (null body)
            ;; Should NS be used in this call?
            (recall-value name)
            (let* ((mx-atom (build-mx-atom expr))
                   (key (mx-atom-name mx-atom)))
              (update-value key mx-atom))))))
