;;;; core.lisp

(uiop:define-package #:streams/core
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:read-term
           #:read-path
           #:resolve-path
           #:write-term
           #:dispatch))

(in-package #:streams/core)

(defun valid-id-p (key)
  "Return true if KEY is a valid identifier for mx-atoms."
  (marie:when* (cl-ppcre:scan "^([a-zA-Z]+)(-?[a-zA-Z0-9])*$" key)))

(defun valid-key-p (key)
  "Return true if KEY is a valid key for an mx-atom."
  (let ((v (marie:string-convert key)))
    (valid-id-p v)))

(defun entity-string (id)
  "Return the corresponding universe name from ID, where ID is either a single
character or a string to designate an entity."
  (let ((aliases (mapcar #'car +namespace-list+)))
    (cdr (assoc id aliases :test #'equal))))

(defun table-name (ns &optional package)
  "Return the corresponding table of NS from the universe."
  (let ((name (entity-string ns)))
    (marie:hyphenate-intern package name "table")))

(defun namespace-table (namespace)
  "Return the table indicated by NAMESPACE."
  (let* ((function (table-name namespace :streams/classes))
         (table (funcall function *mx-universe*)))
    table))

(defun namespace-hash (key namespace)
  "Return the value stored in the corresponding table of NAMESPACE under KEY."
  (gethash key (namespace-table namespace)))

(defun (setf namespace-hash) (value key namespace)
  "Set the table slot value specified by KEY and NAMESPACE to VALUE."
  (let ((table (namespace-table namespace)))
    (setf (gethash key table) value)))

(defun namespace-pairs (chain)
  "Return a list of namespace-key pairs from CHAIN, where the first element of
the pair is the namespace marker and the second element of the pair is the key"
  (marie:partition chain 2))

(defun ns-member-p (elem ns-list)
  "Return true if elem is a MEMBER of NS-LIST by CAR."
  (marie:when* (member elem ns-list :key #'car :test #'equal)))

(defun base-namespace-p (ns)
  "Return true if NS is a base namespace indicator."
  (ns-member-p ns +base-namespace-list+))

(defun sub-namespace-p (ns)
  "Return true if NS is sub namespace indicator."
  (ns-member-p ns +sub-namespace-list+))

(defun namespacep (ns)
  "Return true if NS is a namespace indicator."
  (marie:rmap-or ns #'base-namespace-p #'sub-namespace-p))

(defun sub-atom-index (path)
  "Return true if PATH is a sub-atom path."
  (when (and (consp path)
             (not (uiop:emptyp path)))
    (destructuring-bind (ns &optional &rest body)
        path
      (declare (ignorable body))
      (when (namespacep ns)
        (position-if #'sub-namespace-p path :from-end t)))))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (marie:when-let ((index (sub-atom-index path)))
    (subseq path index)))

(defun sub-atom-path-p (path)
  "Return true if PATH starts with a sub-atom path."
  (destructuring-bind (ns &optional &rest body)
      path
    (declare (ignore body))
    (sub-namespace-p ns)))

(defun sub-atom-path-p* (path)
  "Retun true if PATH contains a sub-atom path and PATH is not a sub-atom path
itself."
  (marie:when* (sub-atom-index path) (not (sub-atom-path-p path))))

(defun read-term (term &optional
                         (atom-table (atom-table *mx-universe*))
                         (sub-atom-table (sub-atom-table *mx-universe*)))
  "Return the value specified by TERM in SOURCE."
  (block nil
    (destructuring-bind (path &optional &rest params)
        term
      (declare (ignore params))
      (let ((path (if (key-indicator-p (marie:last* path))
                      path
                      (append path '("=")))))
        (labels ((fn (location value)
                   (cond ((null location) value)
                         (t (let ((val (gethash (car location) value)))
                              (if val
                                  (fn (cdr location) val)
                                  (return nil)))))))
          (if (sub-atom-path-p path)
              (fn path sub-atom-table)
              (fn path atom-table)))))))

(defun read-path (path &optional
                         (atom-table (atom-table *mx-universe*))
                         (sub-atom-table (sub-atom-table *mx-universe*)))
  "Return the value specified by PATH in SOURCE."
  (read-term (list path nil) atom-table sub-atom-table))

(defun resolve-path (path atom-table sub-atom-table)
  "Return the final value read from PATH in SOURCE."
  (marie:when-let ((value (read-path path atom-table sub-atom-table)))
    (destructuring-bind (ns &optional &rest body)
        value
      (declare (ignore body))
      (when ns
        (read-path value atom-table sub-atom-table)))))

(defun key-indicator-p (key)
  "Return true if KEY is one of the key indicators for table values."
  (marie:when* (member key +key-indicators+ :test #'equal)))

(defun save-value (location table value)
  "Store VALUE using LOCATION as key in TABLE."
  (let ((v (cond ((sub-atom-path-p* value) (sub-atom-path value))
                 (t value))))
    (setf (gethash (marie:stem location) table) v)))

(defun spawn-table (location table)
  "Conditionally return a new table for term writing and use location as key for
the new table."
  (if (hash-table-p (gethash (car location) table))
      (gethash (car location) table)
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash (car location) table) ht)
        ht)))

(defun write-term (term atom-table sub-atom-table &key whole)
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional params)
      term
    (labels ((fn (location flag atom-tab sub-atom-tab)
               (cond ((null location)
                      (fn '("=") flag atom-tab sub-atom-tab))
                     ((and (marie:solop location)
                           (key-indicator-p (marie:stem location)))
                      (save-value location atom-tab (if whole params (car params)))
                      (when flag
                        (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab)))
                     (t (fn (cdr location) flag (spawn-table location atom-tab) sub-atom-tab)))))
      (fn path (sub-atom-path-p* path) atom-table sub-atom-table)
      (read-term term atom-table sub-atom-table))))

(defun empty-params-p (params)
  "Return true if PARAMS is considered empty."
  (or (null params)
      (every #'null params)))

(defun find-table (table)
  "Return the table from the universe identified by TABLE."
  (funcall table *mx-universe*))

(defun valid-terms-p (form)
  "Return true if FORM is a valid MSL form."
  (cond ((stringp form) nil)
        (t (destructuring-bind (&optional head &rest body)
               form
             (declare (ignore body))
             (marie:when*
               (consp head)
               (destructuring-bind (value &rest rest)
                   head
                 (declare (ignore rest))
                 (and (consp value)
                      (namespacep (first value)))))))))

(defun dispatch (expr)
  "Evaluate EXPR as an MSL expression and store the resulting object in the
universe."
  (let ((terms (if (consp expr) expr (streams/expr:parse-msl expr)))
        (atom-tab (find-table #'atom-table))
        (sub-atom-tab (find-table #'sub-atom-table)))
    (when terms
      (streams/logger:write-log expr)
      (loop :for term :in terms
            :collect
            (destructuring-bind (path &optional &rest params)
                term
              (cond ((empty-params-p params)
                     (read-term (list path params) atom-tab sub-atom-tab))
                    (t (let ((values (write-term (list path params) atom-tab sub-atom-tab)))
                         (when (consp values)
                           (loop :for value :in values
                                 :when (valid-terms-p value)
                                   :do (dispatch value)))
                         values))))))))

