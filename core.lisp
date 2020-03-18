;;;; core.lisp

(uiop:define-package #:streams/core
  (:use #:cl)
  (:export #:store-msl))

(in-package #:streams/core)

(defun mx-atom-p (data)
  "Return true if DATA is an mx-atom instance."
  (typep data 'streams/classes:mx-atom))

(defun keyword-intern (symbol)
  "Intern the symbol SYMBOL in the keyword package."
  (intern symbol (find-package :keyword)))

(defun tokenize-expr (data)
  "Tokenize DATA."
  data)

(defun upcase-keyword (keyword)
  "Return an upcased version of KEYWORD."
  (if (keywordp keyword)
      (keyword-intern (string-upcase (marie:string-convert keyword)))
      keyword))

(defun upcase-keywords (list)
  "Return a list wherein all keyword elements are upcased."
  (mapcar #'upcase-keyword list))

(defun valid-id-p (key)
  "Return true if KEY is a valid identifier for mx-atoms."
  (when (cl-ppcre:scan "^([a-zA-Z]+)(-?[a-zA-Z0-9])*$" key)
    t))

(defun valid-key-p (key)
  "Return true if KEY is a valid key for an mx-atom."
  (let ((v (marie:string-convert key)))
    (valid-id-p v)))

(defun key (value)
  "Extract the key used in VALUE."
  (destructuring-bind (ns key &optional &body body)
      value
    (declare (ignore ns body))
    key))

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

(defun single-recall-p (metadata)
  "Return true if there is only a single recall in METADATA. That is,
((\"birthday\")) is a single recall, while ((\"birthday\") (\"state\")) is
not. "
  (destructuring-bind (m &body body)
      metadata
    (and (null body)
         (marie:solop m))))

(defun all-recall-p (metadata)
  "Return true if all the items in METADATA are for recalling values. That
is,((\"birthday\") (\"state\")) is an all recall, while ((\"birthday\")) is
not. "
  (every #'marie:solop metadata))

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

(defmacro define-mod-checker (name type &optional doc)
  "Define a mod checker."
  `(defun ,name (mod)
     ,(when doc doc)
     (destructuring-bind (head &optional &rest body)
         mod
       (declare (ignore body))
       (unless (listp head)
         (string= head ,type)))))

(define-mod-checker regex-mod-p
  "/"
  "Return true if MOD is a regex.")

(define-mod-checker bracketed-transform-mod-p
  "[]"
  "Return true if MOD is bracketed transform.")

(defun simple-mod-p (mod)
  "Return true if MOD is either a regex or bracketed-transform mod."
  (marie:f-or mod
              #'regex-mod-p
              #'bracketed-transform-mod-p))

(defun build-z-mods (mods)
  "Return a collection of simple mods from MODS."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   (t (fn (cdr args)
                          (destructuring-bind (head body)
                              (car args)
                            (acons (list "z" head) (list body) acc)))))))
    (fn mods nil)))

(defun normalize-mods (mods)
  "Reformat MODs and return a list of mods for normal atom processing."
  (loop :for mod :in mods
        :when (simple-mod-p mod)
          :collect mod :into simple-mods
        :unless (simple-mod-p mod)
          :collect mod :into real-mods
        :finally (return (append (build-z-mods simple-mods)
                                 real-mods))))

(defun make-value (&rest args)
  "Return a new table containing all information about an atom."
  (destructuring-bind (value mods meta hash comment)
      args
    (declare (ignore comment))
    (let ((table (make-hash-table #'equal)))
      (when value (setf (gethash "=" table) value))
      (when hash (setf (gethash "#" table) hash)))))

(defun on-atom-p (groups)
  "Return true if GROUPS should be stored locally on the atom."
  (let ((last (marie:last* groups)))
    (when (marie:solop last)
      t)))

(defun sub-namespace-aliases ()
  "Return the list of namespaces under an atom."
  (loop :for ns-spec :in streams/specials:*namespace-list*
        :when (destructuring-bind (alias name rank)
                     ns-spec
                   (declare (ignore alias name))
                (= rank 9))
          :collect (first ns-spec)))

(defun on-universe-p (groups)
  "Return true if GROUPS should be stored globally on the universe."
  (let ((last (marie:last* groups)))
    (or (destructuring-bind (ns &optional key)
            last
          (declare (ignore key))
          (when (member ns (sub-namespace-aliases) :test #'equal)
            t))
        (not (on-atom-p groups)))))

(defun store-on-atom (groups params &optional force)
  "Store the value specified in GROUPS and PARAMS. If FORCE is true, a new atom
will be reallocated on the universe."
  (destructuring-bind ((ns key) location)
      groups
    (destructuring-bind (params-head &rest params-body)
        params
      (declare (ignore params-body))
      (let ((mx-atom (streams/classes:make-mx-atom (list ns key)
                                                   (make-hash-table :test #'equal)
                                                   force)))
        (destructuring-bind (loc &rest rest)
            location
          (declare (ignore rest))
          (setf (gethash loc (streams/classes:value mx-atom))
                params-head))))))

(defun store-msl (expr &optional force)
  "Parse EXPR as an MSL expression and store the resulting object in the universe."
  (let ((value-list expr))        ;(streams/expr:parse-msl expr)
    (loop :for value :in value-list
          :do (destructuring-bind (path &optional &rest params)
                  value
                (let ((groups (path-groups path)))
                  (cond ((on-atom-p groups) (store-on-atom groups params force))
                        (t nil)))))))
