;;;; core.lisp

(uiop:define-package #:streams/core
  (:use #:cl)
  (:export #:store-msl))

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

(defun on-atom-p (groups)
  "Return true if GROUPS should be stored locally on the atom."
  (let ((last (marie:last* groups)))
    (when (marie:solop last)
      t)))

(defun sub-namespace-p (ns)
  "Return true if NS is sub-namespace."
  (when (member ns streams/specials:*sub-namespace-aliases* :test #'equal)
    t))

(defun on-universe-p (groups)
  "Return true if GROUPS should be stored globally on the universe."
  (let ((last (marie:last* groups)))
    (or (destructuring-bind (ns &optional key)
            last
          (declare (ignore key))
          (sub-namespace-p ns))
        (not (on-atom-p groups)))))

(defun recall (key location &optional (universe-table #'streams/classes:atom-table))
  "Retrieve an atom value from the universe as specified by KEY and LOCATION."
  (let* ((table (funcall universe-table streams/specials:*mx-universe*))
         (value (streams/classes:value (gethash key table))))
    (gethash location value)))

(defun dispatch-on-atom (groups &optional params force)
  "Store the value specified in GROUPS and PARAMS. If FORCE is true, a new atom
will be reallocated on the universe."
  (destructuring-bind ((ns key) place)
      groups
    (destructuring-bind (&optional params-head &rest params-body)
        params
      (declare (ignore params-body))
      (let ((location (first place)))
        (cond ((or (null params) (null params-head))
               (recall key location))
              (t (let ((mx-atom (streams/classes:make-mx-atom (list ns key)
                                                              (make-hash-table :test #'equal)
                                                              force)))
                   (setf (gethash location (streams/classes:value mx-atom))
                         params-head))))))))

(defun dispatch-on-universe (groups &optional params force)
  "Store the value specified in GROUPS and PARAMS. If FORCE is true, a new atom
will be reallocated on the universe."
  (destructuring-bind (place (ns key))
      groups
    (declare (ignore place))
    (destructuring-bind (&optional params-head &rest params-body)
        params
      (declare (ignore params-body))
      (cond ((or (null params) (null params-head))
             ;; recall here
             nil)
            (t (let ((mx-atom (streams/classes:make-mx-atom (list ns key)
                                                            (make-hash-table :test #'equal)
                                                            force)))
                 (setf (gethash key (streams/classes:value mx-atom))
                       params-head)))))))

(defun dispatch-0 (expr &optional force)
  "Parse EXPR as an MSL expression and store the resulting object in the
universe."
  (let ((terms expr))        ;(streams/expr:parse-msl expr)
    (loop :for term :in terms
          :collect (destructuring-bind (path &optional &rest params)
                       term
                     (let ((groups (path-groups path)))
                       (cond ((on-atom-p groups) (dispatch-on-atom groups params force))
                             ((on-universe-p groups) (dispatch-on-universe groups params force))
                             (t 'else)))))))

(defun sub-atom-path-p (path)
  "Return true if PATH is a sub-atom."
  (destructuring-bind (&optional ns key sub-ns sub-key)
      path
    (declare (ignore ns key sub-key))
    (sub-namespace-p sub-ns)))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (subseq path 2))

(defun write-chain (term &optional (destination (make-hash-table :test #'equal)))
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional &rest params)
      term
    (labels ((fn (args tab)
               (cond ((marie:solop args)
                      (progn (setf (gethash (marie:stem args) tab) params)
                             destination))
                     (t (fn (cdr args)
                            (if (hash-table-p (gethash (car args) tab))
                                (gethash (car args) tab)
                                (let ((ht (make-hash-table :test #'equal)))
                                  (setf (gethash (car args) tab) ht)
                                  ht)))))))
      (fn path destination))))

(defun read-chain (term source)
  "Return the value specified by PATH starting from SOURCE."
  (destructuring-bind (path &optional &rest params)
      term
    (declare (ignore params))
    (labels ((fn (args tab)
               (cond ((null args) tab)
                     (t (fn (cdr args)
                            (gethash (car args) tab))))))
      (fn path source))))

(defun dump-chain (chain)
  "Print information about SOURCE recursively."
  (marie:dump-table* chain))

(defun dispatch (expr)
  "Parse EXPR as an MSL expression and store the resulting object in the
universe."
  (flet ((fn (path params table)
           (write-chain (list path params)
                        (funcall table streams/specials:*mx-universe*))))
    (let ((terms expr))        ;(streams/expr:parse-msl expr)
      (loop :for term :in terms
            :collect
            (destructuring-bind (path &optional &rest params)
                term
              (cond ((sub-atom-path-p path)
                     (fn (sub-atom-path path) params #'streams/classes:sub-atom-table))
                    ((not (sub-atom-path-p path))
                     (fn path params #'streams/classes:atom-table))
                    (t nil)))))))

