;;;; core.lisp

(uiop:define-package #:streams/core
  (:use #:cl)
  (:export #:store-msl
           #:dump-msl
           #:dump-metadata
           #:eval-msl))

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

(defun namespace-table (namespace)
  "Return the table indicated by NAMESPACE."
  (let* ((function (streams/classes:table-name namespace :streams/classes))
         (table (funcall function streams/specials:*mx-universe*)))
    table))

(defun namespace-hash (key namespace)
  "Return the value stored in the corresponding table of NAMESPACE under KEY."
  (gethash key (namespace-table namespace)))

(defun (setf namespace-hash) (value key namespace)
  "Set the table slot value specified by KEY and NAMESPACE to VALUE."
  (let ((table (namespace-table namespace)))
    (setf (gethash key table) value)))

(defun update-metadata (obj spec)
  "Update mx-atom OBJ with the list SPEC. The first value of SPEC is a list of
two elements where the first element is the type of the table and the second
element is the key. The second value of SPEC is a either a string, integer, or
character."
  (destructuring-bind ((table key) &optional value)
      spec
    (let* ((metadata (streams/classes:metadata obj))
           (metatable (gethash table metadata)))
      (cond ((null value) (gethash key metatable))
            (value (setf (gethash key metatable) value))
            (t nil)))))

(defun dump-metadata (obj)
  "Display information about the metadata stored in OBJ."
  (let ((table (streams/classes:metadata obj)))
    (loop :for k :being :the :hash-keys :in table
          :for v :being :the :hash-values :in table
          :do (progn
                (format t "* ~S~%" k)
                (marie:dump-table v)))))

;; (defun eval-expr (expr)
;;   "Evaluate EXPR as a complete MSL expression, store the result into the active
;; namespace, then return the mx-atom instance and the corresponding table as
;; multiple values."
;;   (block nil
;;     (let ((expr expr))                  ;(parse ...)
;;       (if expr
;;           (macrolet ((vt (v) `(values ,v)))
;;             (destructuring-bind ((ns key) &optional value metadata hash comment)
;;                 expr
;;               (multiple-value-bind (v existsp)
;;                   (namespace-hash key ns)
;;                 (when existsp
;;                   (bind-slots v hash comment))
;;                 (cond
;;                   ;; (@walt)
;;                   ;; ‘walt’ still does not exist
;;                   ((and (null existsp) (null value))
;;                    nil)
;;                   ;; (@walt)
;;                   ;; ‘walt’ already exists
;;                   ((and existsp (null value) (null metadata))
;;                    (vt v))
;;                   ;; (@walt :age)
;;                   ;; ‘walt’ exists, and there’s only one metadata recall
;;                   ((and existsp (null value) (single-recall-p metadata))
;;                    (let ((item (assoc (caar metadata) (streams/classes:metadata v))))
;;                      (when item
;;                        (vt v))))
;;                   ;; (@walt :age :gender)
;;                   ;; ‘walt’ exists, there is no value, all metadata are recalls
;;                   ((and existsp (null value) (all-recall-p metadata))
;;                    (vt v))
;;                   ;; (@walt "Walt Disney" :age :gender)
;;                   ;; ‘walt’ exists, there are p-values, and all the s-values are recalls
;;                   ((and existsp value (all-recall-p metadata))
;;                    (bind-slots v value)
;;                    (vt v))
;;                   ;; (@walt "Walt Disney") | (@walt :age 65) | (@walt "Walt Disney" :age 65)
;;                   ;; ‘walt’ exists, and either p-values or s-values exists
;;                   ((and existsp (or value metadata))
;;                    (bind-slots v value)
;;                    (when metadata
;;                      (setf (streams/classes:metadata v)
;;                            (union (streams/classes:metadata v) metadata :key #'car :test #'equal)))
;;                    (vt v))
;;                   ;; (@walt "Walt Disney" :age 65)
;;                   ;; ‘walt’ does not exist and we’re creating a new instance
;;                   (t (let ((o (streams/classes:make-mx-atom ns key value metadata hash comment)))
;;                        (setf (namespace-hash key ns) o)
;;                        (vt o)))))))
;;           (return nil)))))

(defun namespace-pairs (chain)
  "Return a list of namespace-key pairs from CHAIN, where the first element of
the pair is the namespace marker and the second element of the pair is the key"
  (marie:partition chain 2))

(defun namespace-symbol-p (symbol)
  "Return true if SYMBOL is a valid namespace character."
  (let ((sym (intern (string symbol) (find-package :streams/specials))))
    (when (member sym streams/specials:*namespaces*)
      t)))

(defun namespace-pairs-p (pairs)
  "Return true if PAIRS is a valid namespace pairs."
  (every #'(lambda (pair) (namespace-symbol-p (first pair)))
         pairs))

(defun namespace-rank (ns)
  "Return the rank of NS as an integer. The lower the value the higher the rank."
  (let* ((string (marie:string-convert ns))
         (sym (intern string (find-package :streams/specials))))
    (position sym streams/specials:*namespaces*)))

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
          ((marie:solop ranks) t)
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

(defun build-mx-atom-metadata (metadata)
  "Return a list of MX-ATOM-METADATA instances from METADATA."
  (flet ((fn (args)
           (apply #'streams/classes:make-mx-atom-metadata args)))
    (when metadata
      (mapcar #'fn metadata))))

(defun build-mx-atom-data (&rest data)
  "Return an MX-ATOM-DATA instance from DATA."
  (when data
    (apply #'streams/classes:make-mx-atom-data data)))

(defun store-msl (expr)
  "Parse EXPR as MSL and store the resulting object in the universe."
  (multiple-value-bind (value presentp successp)
      (streams/expr::parse-msl expr)
    (if (and value presentp successp)
        (destructuring-bind ((ns key) &optional value mods metadata hash comment)
            value
          (multiple-value-bind (v existsp)
              (namespace-hash key ns)
            (cond (existsp v)
                  (t (let* ((m (build-mx-atom-metadata metadata))
                            (d (build-mx-atom-data (list ns key) value mods m hash comment)))
                       d)))))
        nil)))

(defun dump-msl (expr)
  "Print infromation about EXPR as parsed MSL."
  (streams/etc:dump-object (store-msl expr)))

(defun dump-metadata (mx-atom)
  "Print information about the METADATA slot of MX-ATOM."
  (marie:when-let ((metadata (streams/classes:metadata mx-atom)))
    (loop :for m :in metadata :do (streams/etc:dump-object m))))
