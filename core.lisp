;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:nicknames #:s/core)
  (:export #:eval-expr
           #:show
           #:dump))

(in-package #:streams/core)

(defun mx-atom-p (data)
  "Return true if DATA is an mx-atom instance."
  (typep data 'streams/channels:mx-atom))

(defun keyword-intern (symbol)
  "Intern the symbol SYMBOL in the keyword package."
  (intern symbol (find-package :keyword)))

(defun tokenize-expr (data)
  "Tokenize DATA."
  data)

(defun upcase-keyword (keyword)
  "Return an upcased version of KEYWORD."
  (if (keywordp keyword)
      (keyword-intern (string-upcase (streams/common:string-convert keyword)))
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
  (let ((v (streams/common:string-convert key)))
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
    (intern (streams/common:string-convert symbol) package)))

(defmacro bind-slots (v &rest slots)
  "Set the value of SLOTS in V, to the respective values in the surrounding
scope, with the same names."
  (mof:with-gensyms (obj)
    `(let ((,obj ,v))
       (progn
         ,@(loop :for slot :in slots :collect
                    `(when ,slot (setf (,(intern-symbol slot :streams/channels) ,obj)
                                       ,slot)))))))

(defun single-recall-p (metadata)
  "Return true if there is only a single recall in METADATA. That is,
((\"birthday\")) is a single recall, while ((\"birthday\") (\"state\")) is
not. "
  (destructuring-bind (m &body body)
      metadata
    (and (null body)
         (mof:solop m))))

(defun all-recall-p (metadata)
  "Return true if all the items in METADATA are for recalling values. That
is,((\"birthday\") (\"state\")) is an all recall, while ((\"birthday\")) is
not. "
  (every #'mof:solop metadata))

(defun namespace-table (namespace)
  "Return the table indicated by NAMESPACE."
  (let* ((function (streams/channels:table-name namespace :streams/channels))
         (table (funcall function streams/ethers:*mx-universe*)))
    table))

(defun namespace-hash (key namespace)
  "Return the value stored in the corresponding table of NAMESPACE under KEY."
  (gethash key (namespace-table namespace)))

(defun (setf namespace-hash) (value key namespace)
  "Set the table slot value specified by KEY and NAMESPACE to VALUE."
  (let ((table (namespace-table namespace)))
    (setf (gethash key table) value)))

(defun metadata-specifier-p (prefix)
  "Return true if PREFIX is a valid identifier for a subatomic namespace."
  (when (member prefix streams/ethers:*metadata-prefixes* :test #'equal)
    t))

(defun update-metadata (obj spec)
  "Update mx-atom OBJ with the list SPEC. The first value of SPEC is a list of
two elements where the first element is the type of the table and the second
element is the key. The second value of SPEC is a either a string, integer, or
character."
  (destructuring-bind ((table key) &optional value)
      spec
    (when (metadata-specifier-p table)
      ;; This is already guaranteed to exist because of INITIALIZE-INSTANCE
      (let* ((metadata (streams/channels:metadata obj))
             (metatable (gethash table metadata)))
        (cond ((null value) (gethash key metatable))
              (value (setf (gethash key metatable) value))
              (t nil))))))

(defun dump-metadata (obj)
  "Display information about the metadata stored in OBJ."
  (streams/common:dump-table (streams/channels:metadata obj)))

(defun eval-expr (expr)
  "Evaluate EXPR as a complete MSL expression, store the result into the active
namespace, then return the mx-atom instance and the corresponding table as
multiple values."
  (block nil
    (let ((expr expr))                  ;(parse ...)
      (if expr
          (macrolet ((vt (v) `(values ,v)))
            (destructuring-bind ((ns key) &optional value metadata hash comment)
                expr
              (multiple-value-bind (v existsp)
                  (namespace-hash key ns)
                (when existsp
                  (bind-slots v hash comment))
                (cond
                  ;; (@walt)
                  ;; ‘walt’ still does not exist
                  ((and (null existsp) (null value))
                   nil)
                  ;; (@walt)
                  ;; ‘walt’ already exists
                  ((and existsp (null value) (null metadata))
                   (vt v))
                  ;; (@walt :age)
                  ;; ‘walt’ exists, and there’s only one metadata recall
                  ((and existsp (null value) (single-recall-p metadata))
                   (let ((item (assoc (caar metadata) (streams/channels:metadata v))))
                     (when item
                       (vt v))))
                  ;; (@walt :age :gender)
                  ;; ‘walt’ exists, there is no value, all metadata are recalls
                  ((and existsp (null value) (all-recall-p metadata))
                   (vt v))
                  ;; (@walt "Walt Disney" :age :gender)
                  ;; ‘walt’ exists, there are p-values, and all the s-values are recalls
                  ((and existsp value (all-recall-p metadata))
                   (bind-slots v value)
                   (vt v))
                  ;; (@walt "Walt Disney") | (@walt :age 65) | (@walt "Walt Disney" :age 65)
                  ;; ‘walt’ exists, and either p-values or s-values exists
                  ((and existsp (or value metadata))
                   (bind-slots v value)
                   (when metadata
                     (setf (streams/channels:metadata v)
                           (union (streams/channels:metadata v) metadata :key #'car :test #'equal)))
                   (vt v))
                  ;; (@walt "Walt Disney" :age 65)
                  ;; ‘walt’ does not exist and we’re creating a new instance
                  (t (let ((o (streams/channels:make-mx-atom ns key value metadata hash comment)))
                       (setf (namespace-hash key ns) o)
                       (vt o)))))))
          (return nil)))))

(defun namespace-pairs (chain)
  "Return a list of namespace-key pairs from CHAIN, where the first element of
the pair is the namespace marker and the second element of the pair is the key"
  (mof:partition chain 2))

(defun namespace-symbol-p (symbol)
  "Return true if SYMBOL is a valid namespace character."
  (let ((sym (intern (string symbol) (find-package :streams/ethers))))
    (when (member sym streams/ethers:*namespaces*)
      t)))

(defun namespace-pairs-p (pairs)
  "Return true if PAIRS is a valid namespace pairs."
  (every #'(lambda (pair) (namespace-symbol-p (first pair)))
         pairs))

(defun namespace-rank (ns)
  "Return the rank of NS as an integer. The lower the value the higher the rank."
  (let* ((string (streams/common:string-convert ns))
         (sym (intern string (find-package :streams/ethers))))
    (position sym streams/ethers:*namespaces*)))

(defun either-zero-p (x y)
  "Return true if either X or Y is a zero."
  (or (zerop x) (zerop y)))

(defun rank-greater-p (ns1 ns2)
  "Return true if NS1 has a higher rank than NS2, that is, the integer value of
NS1 is less than the integer value of NS2."
  (cond ((either-zero-p ns1 ns2) t)
        (t (< ns1 ns2))))

(defun valid-ranks-p (ranks)
  "Return true if RANKS is a valid sequencing of namespace ranks."
  (labels ((fun (r)
             (cond ((and (first r) (null (second r)))
                    t)
                   ((null r) t)
                   ((not (rank-greater-p (first r) (second r)))
                    nil)
                   (t (fun (cdr r))))))
    (cond ((null ranks) nil)
          ((mof:solop ranks) t)
          (t (fun ranks)))))

(defun namespace-chain-p (chain)
  "Return true if CHAIN is a valid chaining of namespaces, wherein the namespace
ranks are in the correct order."
  (when (evenp (length chain))
    (let ((pairs (namespace-pairs chain)))
      (when (namespace-pairs-p pairs)
        (let ((ranks (loop :for pair :in pairs :collect (namespace-rank (first pair)))))
          (valid-ranks-p ranks))))))

(defun compose-namespaces (path)
  "Return a namespace chain object by linearly composing the namespace path from
NAMESPACES. The object returned contains complete namespace traversal information."
  (when (namespace-chain-p path)
    (let ((pairs (namespace-pairs path)))
      pairs)))

(defmacro define-dynamic-constant (name value)
  "Bind NAME to VALUE and only change the binding after subsequent calls to the macro."
  `(handler-bind ((sb-ext:defconstant-uneql #'(lambda (c)
                                                (let ((r (find-restart 'continue c)))
                                                  (when r
                                                    (invoke-restart r))))))
     (defconstant ,name ,value)))
