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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-symbol (symbol package)
    "Set the home package of SYMBOL to PACKAGE, ensuring that SYMBOL is indeed a symbol."
    (intern (marie:string-convert symbol) package)))

(defmacro bind-slots (v &rest slots)
  "Set the value of SLOTS in V, to the respective values in the surrounding
scope, with the same names."
  (marie:with-gensyms (obj)
    `(let ((,obj ,v))
       (progn
         ,@(loop :for slot :in slots :collect
                 `(when ,slot (setf (,(intern-symbol slot :streams/classes) ,obj)
                                    ,slot)))))))

(defun entity-string (id)
  "Return the corresponding universe name from ID, where ID is either a single
character or a string to designate an entity."
  (cdr (assoc id streams/specials:*namespace-aliases* :test #'equal)))

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

(defun path-groups (path)
  "Return a namespace chain object by linearly composing the namespace path from
NAMESPACES. The object returned contains complete namespace traversal information."
  (let ((pairs (namespace-pairs path)))
    pairs))

(defun sub-namespace-p (ns)
  "Return true if NS is sub-namespace."
  (when (member ns streams/specials:*sub-namespace-aliases* :test #'equal)
    t))

(defun recall (key location &optional (universe-table #'streams/classes:atom-table))
  "Retrieve an atom value from the universe as specified by KEY and LOCATION."
  (let* ((table (funcall universe-table streams/specials:*mx-universe*))
         (value (streams/classes:value (gethash key table))))
    (gethash location value)))

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

(defun write-term (term &optional (destination (make-hash-table :test #'equal)))
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional &rest params)
      term
    (labels ((save (args tab value)
               (setf (gethash (marie:stem args) tab)
                     value))
             (fn (args flag tab)
               (cond ((marie:solop args)
                      (cond (flag (save args tab (sub-atom-path path))
                                  (fn (sub-atom-path path) nil destination)
                                  destination)
                            (t (save args tab (marie:stem params))
                               destination)))
                     (t (fn (cdr args)
                            flag
                            (if (hash-table-p (gethash (car args) tab))
                                (gethash (car args) tab)
                                (let ((ht (make-hash-table :test #'equal)))
                                  (setf (gethash (car args) tab) ht)
                                  ht)))))))
      (cond ((sub-atom-path-p path)
             (fn path t destination))
            (t (fn path nil destination)))
      (read-term term destination))))

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

(defun sub-atom-path-p (path)
  "Return true if PATH is a sub-atom."
  (destructuring-bind (&optional ns key sub-ns sub-key)
      path
    (declare (ignore ns key sub-key))
    (sub-namespace-p sub-ns)))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (subseq path 2))

(defun dispatch (expr)
  "Parse EXPR as an MSL expression and store the resulting object in the
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
