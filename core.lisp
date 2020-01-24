;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:nicknames #:s/core))

(in-package #:streams/core)

(defun mx-atom-key (mx-atom)
  "Return the name used to identify MX-ATOM."
  (streams/channels:key mx-atom))

(defun mx-atom-value (mx-atom)
  "Return the first value used to identify MX-ATOM."
  (streams/channels:value mx-atom))

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

(defun data-marker-p (v)
  "Return true if V is a valid mx-atom data."
  (or (and (symbolp v) (not (keywordp v)))
      (stringp v)
      (numberp v)
      (consp v)
      (pathnamep v)))

(defun metadata-marker-p (v)
  "Return true if V is a valid mx-atom metadata."
  (not (data-marker-p v)))

(defun metadata-marker-count (expr)
  "Return the number of metadata markers in LIST."
  (count-if #'metadata-marker-p expr))

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
              (char= (elt string 0) prefix)))))

(defun infixedp (symbol infix)
  "Return true if SYMBOL contains the infix INFIX."
  (and (symbolp symbol)
       (let* ((string (streams/common:string-convert symbol))
              (length (length string)))
         (and (>= length 3)
              (not (char= (elt string 0) infix))
              (not (char= (elt string (1- length)) infix))
              (find infix string)))))

(defun @-prefixed-p (symbol)
  "Return true if SYMBOL is prefixed with the @ character."
  (prefixedp symbol #\@))

(defun :-infixed-p (symbol)
  "Return true if SYMBOL is infixed with the : character."
  (infixedp symbol #\:))

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

(defun normalize-expr (expr)
  "Pass EXPR through a filter, returning a new expr with correct structure."
  (split-prefixes expr))

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
  "Return true if KEY is a valid key for an mx-atom."
  (let ((v (streams/common:string-convert key)))
    (valid-id-p v)))

(defun key (value)
  "Extract the key used in VALUE."
  (destructuring-bind (ns key &optional &body body)
      value
    (declare (ignore ns body))
    key))

(defun valid-keys-p (expr)
  "Return true if the keys in EXPR are valid."
  (labels ((fn (args)
             (cond ((null args) t)
                   ((consp (car args)) (fn (car args)))
                   ((unless (valid-key-p (car args))) nil)
                   (t (fn (cdr args))))))
    (and (valid-key-p (key expr))
         (fn expr))))

(defun valid-form-p (expr)
  "Return true if EXPR is a valid mx-atom expression."
  ;; The first pass goes through all the items and checks for the keys
  (and (valid-keys-p expr)))

(defun read-expr (expr)
  "Read EXPR and break it down into multiple values."
  (let ((expr (normalize-expr expr)))
    (destructuring-bind (ns key &optional &rest _)
        expr
      (declare (ignore _))
      (labels ((fn (args value metadata)
                 (cond ((null args) (values ns key value (nreverse metadata)))
                       (t (multiple-value-bind (start end)
                              (bounds args)
                            (fn (nthcdr (1+ end) args)
                                value
                                (acons (car args) (subseq args start (1+ end)) metadata)))))))
        (fn (secondary-values expr) (primary-values expr) nil)))))

(defun read-expr-from-string (raw-expr)
  "Read an EXPR as a string and return values that represent the parsed information."
  (let ((v (streams/common:read-from-string* raw-expr)))
    (when (valid-expr-p v)
      (let ((v (normalize-expr v)))
        (when (valid-form-p v)
          (read-expr v))))))

(defun examine-expr (raw-expr)
  "Print information about RAW-EXPR."
  (loop :for e :in (read-expr-from-string raw-expr) :do (format t "~S~20T~S~%" e (type-of e))))

(defun resolve-atom (atom)
  "Expand the values inside ATOM then assign them to the corresponding stores."
  (declare (ignore atom))
  nil)

(defun build-pairs (items)
  "Group items into pairs."
  (labels ((fn (items acc)
             (cond ((null items) (nreverse acc))
                   ((keywordp (cadr items))
                    (fn (cdr items)
                        (cons (list (first items) nil)
                              acc)))
                   (t (fn (cddr items)
                          (cons (list (first items) (second items))
                                acc))))))
    (fn items nil)))

(defun build-groups (items)
  "Return item groupings from ITEMS according to KEY."
  (when (keywordp (first items))
    (labels ((fn (items acc)
               (cond ((null items) (reverse acc))
                     (t (multiple-value-bind (start end)
                            (bounds items)
                          (declare (ignorable start))
                          (if start
                              (fn (nthcdr (1+ end) items)
                                  (cons (subseq items 0 (1+ end)) acc))
                              (fn (cdr items)
                                  (cons (list (car items) nil) acc))))))))
      (fn items nil))))

(defun build-map (items &key (test #'keywordp) (constructor #'cons))
  "Create key-value mappings from ITEMS."
  (loop :for item :in (build-pairs items)
        :when (funcall test (car item))
        :collect (destructuring-bind (k v)
                     item
                   (funcall constructor k v))))

(defun build-mx-atom (expr)
  "Return an mx-atom instance from EXPR."
  (multiple-value-bind (ns key value metadata)
      (read-expr expr)
    (streams/channels:make-mx-atom ns key value metadata)))

(defmacro with-current-namespace (&body body)
  "Evaluate BODY in the current namespace."
  `(let* ((namespace (yield-namespace))
          (table (streams/channels:table namespace)))
     (declare (ignorable namespace table))
     ,@body))

(defun recall (key)
  "Return a value in the current namespace by KEY."
  (with-current-namespace
    (multiple-value-bind (v presentp)
        (gethash key table)
      (when presentp
        (values v table namespace)))))

(defun update-key (items key value)
  "Update a specific colon selector under KEY with VALUE within ITEMS."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((eql (caar args) key) (fn (cdr args) (acons key value acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (fn items nil)))

(defun update-map (items values)
  "Update all matching colon selectors in ITEMS."
  (labels ((fn (args acc)
             (cond ((null args) acc)
                   (t (fn (cdr args) (update-key (or acc items) (caar args) (cdar args)))))))
    (fn (build-groups values) nil)))

;;; Add the ability to recall
(defun dispatch (key expr)
  "Store or update a value under KEY with EXPR in the active namespace."
  (let* ((p-values (primary-values expr))
         (s-values (secondary-values expr))
         (groups (build-groups s-values)))
    (declare (ignorable groups))
    (with-current-namespace
      (macrolet ((vtn (v) `(values ,v table namespace)))
        (multiple-value-bind (v existsp)
            (gethash key table)
          (if existsp
              (progn
                (when p-values
                  (setf (streams/channels:value v) p-values))
                (when s-values
                  (setf (streams/channels:metadata v)
                        (update-map (streams/channels:metadata v) s-values)))
                (vtn v))
              (let ((v (build-mx-atom expr)))
                (setf (gethash key table) v)
                (vtn v))))))))

(defun eval-expr (expr)
  "Evaluate EXPR, store the result into the active namespace, then return the mx-atom instance, table, and active namespace as multiple values."
  (let* ((expr (normalize-expr expr)))
    (destructuring-bind (ns key &optional &body body)
        expr
      (declare (ignorable ns))
      (cond ((null body) (recall key))
            (t (dispatch key expr))))))

(defun plural-requests-p (s-values)
  "Return true if more than metadata request is present in S-VALUES."
  (> (metadata-marker-count s-values) 1))

(defun empty-requests-p (s-values)
  "Return true if all the items in S-VALUES "
  (every #'(lambda (arg)
             (destructuring-bind (k v)
                 arg
               (declare (ignore k))
               (null v)))
         s-values))

(defun eval-expr-1 (expr)
  "Evaluate EXPR, store the result into the active namespace, then return the mx-atom instance, table, and active namespace as multiple values."
  (let* ((expr (normalize-expr expr))
         (p-values (primary-values expr))
         (s-values (secondary-values expr))
         (groups (build-groups expr)))
    (declare (ignorable p-values s-values groups))
    (destructuring-bind (ns key &optional &body body)
        expr
      (declare (ignorable ns key body))
      ;; If NS is specified, lock the operations to that namespace

      ;; Add ability to recall if a colon metadata is used
      ;; If a colon recall is used return the value that that colon metadata refers to
      ;; If multiple colons are used return all those values, maybe as lists
      ;; Return the metadata value as list of strings, maybe.

      ;; Implement operation compounding, that is, value recall and setting can be done
      ;; in one operation

      ;; BODY can be seen as a list of operations

      ;; Find a way to combine recalling and setting

      ;; Dispatch metadata based on the intent

      ;; Should BUILD-GROUPS be applied to S-VALUES regardless of the content?
      ;; Is RECALL dumb?
      (cond ((and (null p-values)
                  (plural-requests-p s-values)
                  (empty-requests-p s-values))
             nil)
            ;; Implement recalling for single metadata requests
            ((null s-values) (recall key))
            (t (dispatch key expr))))))

(defun recall-metadata (key mx-atom)
  "Return the the value specified by KEY in MX-ATOM."
  (let* ((metadata (streams/channels:metadata mx-atom))
         (item (streams/common:assoc-value key metadata)))
    (when item
      (format nil "~{~A~^ ~}" item))))

(defun yield (expr)
  "Return the values as expressed in expr."
  (multiple-value-bind (mx-atom table namespace)
      (eval-expr expr)
    (declare (ignorable mx-atom table namespace))
    nil))

