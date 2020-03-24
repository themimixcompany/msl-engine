;;;; core.lisp

(uiop:define-package #:streams/core
  (:use #:cl
        #:streams/specials)
  (:export #:read-term
           #:read-path
           #:resolve-path
           #:write-term
           #:dispatch))

(in-package #:streams/core)

(defun valid-id-p (key)
  "Return true if KEY is a valid identifier for mx-atoms."
  (when (cl-ppcre:scan "^([a-zA-Z]+)(-?[a-zA-Z0-9])*$" key)
    t))

(defun valid-key-p (key)
  "Return true if KEY is a valid key for an mx-atom."
  (let ((v (marie:string-convert key)))
    (valid-id-p v)))

(defun entity-string (id)
  "Return the corresponding universe name from ID, where ID is either a single
character or a string to designate an entity."
  (let ((aliases (mapcar #'car *namespace-list*)))
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
  (when (member elem ns-list :key #'car :test #'equal)
    t))

(defun namespacep (ns)
  "Return true if NS is a namespace indicator."
  (ns-member-p ns *namespace-list*))

(defun sub-namespace-p (ns)
  "Return true if NS is sub-namespace indicator."
  (ns-member-p ns *sub-namespace-list*))

(defun sub-atom-index (path)
  "Return true if PATH is a sub-atom path."
  (destructuring-bind (ns &optional &rest body)
      path
    (declare (ignorable body))
    (when (namespacep ns)
      (position-if #'sub-namespace-p path :from-end t))))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (marie:when-let ((index (sub-atom-index path)))
    (subseq path index)))

(defun sub-atom-path-p (path)
  "Retun true if PATH contains a sub-atom path."
  (when (sub-atom-index path)
    t))

(defun read-term (term atom-table sub-atom-table)
  "Return the value specified by TERM in SOURCE."
  (block nil
    (destructuring-bind (path &optional &rest params)
        term
      (declare (ignore params))
      (labels ((fn (args atom-tab sub-atom-tab)
                 (cond ((null args) atom-tab)
                       (t (let ((value (gethash (car args) atom-tab)))
                            (if value
                                (fn (cdr args) value sub-atom-tab)
                                (return nil)))))))
        (fn path atom-table sub-atom-table)))))

(defun read-path (path atom-table sub-atom-table)
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

(defun write-term (term atom-table sub-atom-table)
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional &rest params)
      term
    (labels ((save (args tab value)
               (setf (gethash (marie:stem args) tab) value))
             (fn (arg flag atom-tab sub-atom-tab)
               (cond ((marie:solop arg)
                      (cond (flag
                             (save arg atom-tab (cons (sub-atom-path path) params))
                             (fn (sub-atom-path path) nil sub-atom-table sub-atom-table))
                            (t (save arg atom-tab (marie:stem params)))))
                     (t (let ((v (if (hash-table-p (gethash (car arg) atom-tab))
                                       (gethash (car arg) atom-tab)
                                       (let ((ht (make-hash-table :test #'equal)))
                                         (setf (gethash (car arg) atom-tab) ht)
                                         ht))))
                          (fn (cdr arg) flag v sub-atom-table))))))
      (cond ((sub-atom-path-p path)
             (fn path t atom-table sub-atom-table))
            (t (fn path nil atom-table sub-atom-table)))
      (read-term term atom-table sub-atom-table))))

(defun empty-params-p (params)
  "Return true if PARAMS is considered empty."
  (or (null params)
      (every #'null params)))

(defun dispatch (expr)
  "Evaluate EXPR as an MSL expression and store the resulting object in the
universe."
  (flet ((find-table (tab)
           (funcall tab *mx-universe*)))
    (let ((terms expr))        ;(streams/expr:parse-msl expr)
      (loop :for term :in terms
            :collect
            (destructuring-bind (path &optional &rest params)
                term
              (cond ((empty-params-p params)
                     (read-term (list path params)
                                (find-table #'streams/classes:atom-table)
                                (find-table #'streams/classes:sub-atom-table)))
                    (params
                     (write-term (list path params)
                                 (find-table #'streams/classes:atom-table)
                                 (find-table #'streams/classes:sub-atom-table)))
                    (t nil)))))))
