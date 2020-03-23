;;;; core.lisp

(uiop:define-package #:streams/core
  (:use #:cl)
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
  (let ((aliases (mapcar #'car streams/specials:*namespace-list*)))
    (cdr (assoc id aliases :test #'equal))))

(defun table-name (ns &optional package)
  "Return the corresponding table of NS from the universe."
  (let ((name (entity-string ns)))
    (marie:hyphenate-intern package name "table")))

(defun namespace-table (namespace)
  "Return the table indicated by NAMESPACE."
  (let* ((function (table-name namespace :streams/classes))
         (table (funcall function streams/specials:*mx-universe*)))
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

(defun sub-namespace-p (ns)
  "Return true if NS is sub-namespace."
  (when (member ns streams/specials:*sub-namespace-list* :key #'car :test #'equal)
    t))

(defun sub-atom-path-p (path)
  "Return true if PATH is a sub-atom."
  (let ((elem (elt path 2)))
    (when (sub-namespace-p elem)
      t)))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (subseq path 2))

(defun read-term (term source)
  "Return the value specified by TERM in SOURCE."
  (block nil
    (destructuring-bind (path &optional &rest params)
        term
      (declare (ignore params))
      (labels ((fn (args tab)
                 (cond ((null args) tab)
                       (t (let ((value (gethash (car args) tab)))
                            (if value
                                (fn (cdr args)
                                    value)
                                (return nil)))))))
        (fn path source)))))

(defun write-term (term &optional table)
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional &rest params)
      term
    (labels ((save (args tab value)
               (setf (gethash (marie:stem args) tab)
                     value))
             (fn (args flag tab)
               (cond ((marie:solop args)
                      (cond (flag (save args tab (cons (sub-atom-path path) params))
                                  (fn (sub-atom-path path) nil table)
                                  table)
                            (t (save args tab (marie:stem params))
                               table)))
                     (t (let ((v (if (hash-table-p (gethash (car args) tab))
                                     (gethash (car args) tab)
                                     (let ((ht (make-hash-table :test #'equal)))
                                       (setf (gethash (car args) tab) ht)
                                       ht))))
                          (fn (cdr args) flag v))))))
      (cond ((sub-atom-path-p path)
             (fn path t table))
            (t (fn path nil table)))
      (read-term term table))))

(defun read-path (path source)
  "Return the value specified by PATH in SOURCE."
  (read-term (list path nil) source))

(defun resolve-path (path source)
  "Return the final value read from PATH in SOURCE."
  (marie:when-let ((value (read-path path source)))
    (destructuring-bind (ns &optional &rest body)
        value
      (declare (ignore body))
      (when ns
        (read-path value source)))))

(defun empty-params-p (params)
  "Return true if PARAMS is considered empty."
  (or (null params)
      (every #'null params)))

(defun dispatch (expr)
  "Evaluate EXPR as an MSL expression and store the resulting object in the
universe."
  (flet ((read-fn (path params table)
           (let ((tab (funcall table streams/specials:*mx-universe*)))
             (read-term (list path params) tab)))
         (write-fn (path params table)
           (let ((tab (funcall table streams/specials:*mx-universe*)))
             (write-term (list path params) tab))))
    (let ((terms expr))        ;(streams/expr:parse-msl expr)
      (loop :for term :in terms
            :collect
            (destructuring-bind (path &optional &rest params)
                term
              (cond ((empty-params-p params)
                     (read-fn path params #'streams/classes:atom-table))
                    (params
                     (write-fn path params #'streams/classes:atom-table))
                    (t nil)))))))
