;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl #:named-readtables)
  (:nicknames #:s/core)
  (:export #:%eval-expr
           #:eval-expr
           #:show
           #:dump))

(in-package #:streams/core)

;;(in-readtable streams/reader:streams-readtable)

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

(defun mx-atom-p (data)
  "Return true if DATA is an mx-atom instance."
  (typep data 'streams/channels:mx-atom))

(defun data-marker-p (v)
  "Return true if V is a valid mx-atom data."
  (or (and (symbolp v) (not (keywordp v)))
      (stringp v)
      (numberp v)
      (consp v)
      (pathnamep v)
      (mx-atom-p v)))

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

(defun ensure-expr (data)
  "Return an expr representation from DATA."
  (if (stringp data)
      (streams/common:read-from-string* data)
      data))

(defun normalize-expr (expr)
  "Pass EXPR through a filter, returning a new expr with correct structure."
  (split-prefixes (ensure-expr expr)))

(defun primary-values (expr)
  "Return the primary values from EXPR; return NIL if none are found."
  (destructuring-bind (_ __ &optional &body body)
      expr
    (declare (ignore _ __))
    (when (and body (data-marker-p (first body)))
      (multiple-value-bind (start end)
          (bounds body)
        (subseq body start (1+ end))))))

(defun upcase-keyword (keyword)
  "Return an upcased version of KEYWORD."
  (if (keywordp keyword)
      (read-from-string (mof:cat "KEYWORD:" (string-upcase (streams/common:string-convert keyword))))
      keyword))

(defun upcase-keywords (list)
  "Return a list wherein all keyword elements are upcased."
  (mapcar #'upcase-keyword list))

(defun secondary-values (expr)
  "Return the secondary values—metadata, etc—from EXPR; return NIL if none are found."
  (destructuring-bind (_ __ &body body)
      expr
    (declare (ignore _ __))
    (let ((index (position-if #'metadata-marker-p body)))
      (when index
        (upcase-keywords (subseq body index))))))

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
  (when (and (listp items)
             (keywordp (first items)))
    (labels ((fn (items acc)
               (cond ((null items) (reverse acc))
                     ((keywordp (cadr items)) (fn (cdr items)
                                                  (cons (list (car items) nil) acc)))
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

(defun recall (key &optional subkey)
  "Return a value in the current namespace by KEY."
  (declare (ignorable subkey))
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
    (if (assoc key items :test #'equal)
        (fn items nil)
        (acons key value items))))

(defun update-map (items values)
  "Update all matching colon selectors in ITEMS."
  (labels ((fn (args acc)
             (cond ((null args) acc)
                   (t (fn (cdr args) (update-key (or acc items) (caar args) (cdar args)))))))
    (fn (remove-if #'(lambda (group)
                       (null (cadr group)))
                   (build-groups values)) nil)))

(defun merge-metadata (key mx-atom)
  "Return the the value specified by KEY in MX-ATOM."
  (let* ((metadata (streams/channels:metadata mx-atom))
         (item (streams/common:assoc-value key metadata)))
    (when item
      (format nil "~{~A~^ ~}" item))))

(defmacro with-expr-values (&body body)
  "Evaluate BODY in the context of primary and secondary values."
  `(let* ((p-values (primary-values expr))
          (s-values (secondary-values expr))
          (groups (build-groups expr)))
     (declare (ignorable p-values s-values groups))
     (progn ,@body)))

(defun recall-count (s-values)
  "Return the number of recalls in S-VALUES."
  (count-if #'null (build-groups s-values) :key #'second))

(defun single-recall-p (s-values)
  "Return true if there’s only one metadata request in S-VALUES."
  (and (= (recall-count s-values) 1)
       (zerop (install-count s-values))))

(defun all-recall-p (s-values)
  "Return true if all the items in S-VALUES are recalls."
  (every #'keywordp s-values))

(defun install-count (s-values)
  "Return the number of installs in S-VALUES."
  (count-if #'(lambda (x) (not (null x))) (build-groups s-values) :key #'second))

(defun single-install-p (s-values)
  "Return true if there is only one install in S-VALUES."
  (and (zerop (recall-count s-values))
       (= (install-count s-values) 1)))

(defun multi-install-p (s-values)
  "Return true if there are one installs in S-VALUES."
  (> (install-count s-values) 1))

(defun empty-requests-p (s-values)
  "Return true if all the items in S-VALUES "
  (every #'(lambda (arg)
             (destructuring-bind (k v)
                 arg
               (declare (ignore k))
               (null v)))
         s-values))

;;; Should the keys of secondary values be converted to uppercase
(defun dispatch (ns key expr)
  "Store or update a value under KEY with EXPR in the current namespace."
  (declare (ignorable ns))
  (let* ((p-values (primary-values expr))
         (s-values (secondary-values expr)))
    (with-current-namespace
      (macrolet ((vtn (v) `(values ,v table namespace)))
        (multiple-value-bind (v existsp)
            (gethash key table)
          (cond
            ;; (@walt)
            ;; ‘walt’ still does not exist
            ((and (null existsp) (null p-values))
             nil)
            ;; (@walt)
            ;; ‘walt’ already exists
            ((and existsp (null p-values) (null s-values))
             (vtn v))
            ;; (@walt :age)
            ;; ‘walt’ exists, and there’s only one recall
            ((and existsp (null p-values) (single-recall-p s-values))
             (let ((item (assoc (first s-values) (streams/channels:metadata v))))
               (when item
                 (vtn v))))
            ;; (@walt :age :gender)
            ;; ‘walt’ exists, there are no p-values, all the s-values are recalls
            ((and existsp (null p-values) (all-recall-p s-values))
             (vtn v))
            ;; (@walt "Walt Disney" :age :gender)
            ;; ‘walt’ exists, there are p-values, and all the s-values are recalls
            ((and existsp p-values (all-recall-p s-values))
             (setf (streams/channels:value v)
                   p-values)
             (vtn v))
            ;; (@walt "Walt Disney") | (@walt :age 65) | (@walt "Walt Disney" :age 65)
            ;; ‘walt’ exists, and either p-values or s-values exists
            ((and existsp (or p-values s-values))
             (when p-values
               (setf (streams/channels:value v)
                     p-values))
             (when s-values
               (setf (streams/channels:metadata v)
                     (update-map (streams/channels:metadata v) s-values)))
             (vtn v))
            ;; (@walt "Walt Disney" :age 65)
            ;; ‘walt’ does not exist and we’re creating a instance
            (t
             (let ((v (build-mx-atom expr)))
               (setf (gethash key table) v)
               (vtn v)))))))))

(defun %eval-expr (expr)
  "Evaluate EXPR as a free expression, store the result into the active namespace, then return the mx-atom instance, table, and current namespace as multiple values."
  (let ((expr (normalize-expr expr)))
    (destructuring-bind (ns key &optional &body _)
        expr
      (declare (ignore _))
      (dispatch ns key expr))))

(defun mx-atom-form-p (data)
  "Return true if data has the form of an mx-atom."
  (when (listp data)
    (let ((data (normalize-expr data)))
      (and (>= (length data) 2)
           (destructuring-bind (ns key &optional &body _)
               data
             (declare (ignore _))
             (and (string= "@" (streams/common:string-convert ns))
                  (or (symbolp key)
                      (stringp key))))))))

(defun mx-atom-body (expr)
  "Return the body of EXPR."
  (when (mx-atom-form-p expr)
    (destructuring-bind (ns key &optional &body body)
        (normalize-expr expr)
      (declare (ignore ns key))
      (when body body))))

(defun free-expr-present-p (expr)
  "Return true if there is an unresolved mx-atom value in EXPR."
  (when (member-if #'mx-atom-form-p (mx-atom-body expr))
    t))

(defun eval-expr (expr)
  "Evaluate EXPR as a complete expression, store the result into the active namespace, then return the mx-atom instance, table, and current namespace as multiple values."
  (block nil
    (let ((expr (normalize-expr expr)))
      (labels ((fn (args acc)
                 (cond
                   ((null args)
                    (let ((v (%eval-expr (reverse acc))))
                      (if (null v)
                          (return nil)
                          v)))
                   ((mx-atom-form-p (car args))
                    (if (free-expr-present-p (car args))
                        (fn (cdr args) (cons (show (car args) (fn (car args) nil)) acc))
                        (let ((v (%eval-expr (car args))))
                          (if (null v)
                              (return nil)
                              (fn (cdr args) (cons (show (car args) v) acc))))))
                   (t (fn (cdr args) (cons (car args) acc))))))
        (fn expr nil)))))

(defun show (expr &optional mx-atom stream)
  "Return the intended string representation of EXPR."
  (block nil
    (let* ((expr (normalize-expr expr))
           (p-values (primary-values expr))
           (s-values (secondary-values expr)))
      (flet ((v-value (v)
               (streams/channels:value v))
             (m-value (s-values v)
               (streams/common:assoc-value (first s-values) (streams/channels:metadata v))))
        (multiple-value-bind (v table namespace)
            (or mx-atom (eval-expr expr))
          (declare (ignorable table namespace))
          (if (null v)
              (return nil)
              (cond
                ;; (@walt :age 0)
                ;; ‘walt’ exists, and there’s only one install
                ((and (null p-values) (single-install-p s-values))
                 (format stream (mof:join (m-value s-values v))))

                ;; (@walt :age)
                ;; ‘walt’ exists, and there’s only one recall
                ((and (null p-values) (single-recall-p s-values))
                 (format stream (mof:join (m-value s-values v))))

                ;; other expressions
                (t (format stream (mof:join (v-value v)))))))))))

(defun dump (expr)
  "Display information about the result of evaluating EXPR."
  (streams/common:dump-object (eval-expr expr)))
