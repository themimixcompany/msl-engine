;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/dispatcher
        #:streams/sexp
        #:marie))

(in-package #:streams/unparser)

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (let ((keys (loop :for k :being :the :hash-key :in table :collect k))
          (ex '(":")))
      (if (mem* ex keys)
          (append (remove* ex keys) ex)
          keys))))

(defun* (children t) (table &optional object)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p table)
    (let ((keys (table-keys table)))
      (loop :for key :in keys
            :for value = (gethash key table)
            :when (hash-table-p value)
              :collect (if object value key)))))

(defun metadatap (value)
  "Return true if VALUE is the : namespace."
  (when* (consp value) (mem (car value) '(":"))))

(defun modsp (value)
  "Return true if VALUE is a datatype or format form."
  (when* (consp value) (mem (car value) '("d" "f"))))

(defun* (prefixedp t) (value)
  "Return true if VALUE is prefixed by certain namespaces."
  (rmap-or value #'metadatap #'modsp))

(defun marshall (list)
  "Return a list where non-cons items are made conses."
  (mapcar #'(lambda (item)
              (if (consp item) item (list item)))
          list))

(defun flatten-1 (list)
  "Return a list where items in LIST are flattened to one level."
  (reduce #'(lambda (x y)
              (cond ((metadatap y) (append x (list y)))
                    ((modsp y) (append x (list y)))
                    (t (append x y))))
          (marshall list)))

(defun* (wrap t) (list)
  "Return a new list where items in LIST are conditionally listified."
  (mapcar #'(lambda (item)
              (cond ((or (atom item)
                         (and (consp item)
                              (not (prefixedp item))
                              (not (stringp (car item)))))
                     (list item))
                    ((metadatap item)
                     (cons (cat (car item) (cadr item))
                           (cddr item)))
                    (t item)))
          list))

(defun stage (list)
  "Return a new list with preprocessed elements for wrapping and joining."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((modsp (car args))
                    (fn (cdr args)
                        (cons (flatten-list (car args)) acc)))
                   ((metadatap (car args))
                    (fn (cdr args)
                        (cons (flatten-1 (wrap (fn (car args) nil)))
                              acc)))
                   (t (fn (cdr args)
                          (cons (car args) acc))))))
    (fn list nil)))

(defun normalize (list)
  "Return special merging on items of LIST."
  (labels ((fn (val)
             (cond ((metadatap val)
                    (loop :for v :in (cdr val) :collect (cons (car val) v)))
                   (t val))))
    (flatten-1 (mapcar #'fn list))))

(defun make-regex (exprs)
  "Return a list containing raw regex expressions from VALUE."
  (flet ((fn (expr)
           (destructuring-bind (regex &optional env val)
               expr
             (cat "/" regex "/" (or env "")
                  (if val (cat " " val) "")))))
    (mapcar #'fn exprs)))

(defun make-transform (exprs)
  (flet ((fn (expr) (cat "[" expr "]")))
    (mapcar #'fn exprs)))

(defun accumulate (keys acc &optional data)
  "Return an accumulator value suitable for CONSTRUCT."
  (flet ((fn (k a d)
           (cond ((mem k '("=")) a)
                 ((mem k '("/")) (cons (make-regex d) a))
                 ((mem k '("[]")) (cons (make-transform d) a))
                 (t (cons k a)))))
    (destructuring-bind (key &optional &rest _)
        keys
      (declare (ignore _))
      (let ((value (fn key acc data)))
        (cond ((mem key '("/" "[]")) value)
              (t (cons data value)))))))

(defun make-head (list)
  "Return a list with custom head merging."
  (when (consp (cdr list))
    (destructuring-bind (ns &optional &rest _)
        list
      (declare (ignore _))
      (cond ((string= ns "@")
             (cons (cat ns (cadr list)) (cddr list)))
            (t list)))))

(defun* (%%construct t) (tab keys acc)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let ((v (gethash (car keys) tab)))
    (cond ((null keys) (nreverse acc))
          ((hash-table-p v)
           (%%construct tab
                        (cdr keys)
                        (cons (%%construct v
                                           (table-keys v)
                                           (list (car keys)))
                              acc)))
          (t (%%construct tab
                          (cdr keys)
                          (accumulate keys acc v))))))

(defun* (%construct t) (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (when-let* ((ht (gethash key table))
              (entries (or keys (table-keys ht))))
    (loop :for v :in (%%construct ht entries nil)
          :for kv = (make-head (cons key v))
          :when kv :collect (normalize kv))))

(defun* (construct t) (table key &optional keys)
  "Return the original expressions in TABLE under KEYS."
  (mapcar #'flatten-1 (mapcar #'wrap (mapcar #'stage (%construct table key keys)))))

(defun convert (terms)
  "Return the original expression from TERMS."
  (flet ((fn (v)
           (destructuring-bind (((ns key) &rest _) &rest __)
               v
             (declare (ignore _ __))
             (car (construct (atom-table *universe*) ns (list key))))))
    (cond ((valid-terms-p terms #'base-namespace-p) (fn terms))
          (t terms))))

(defun* (%collect t) (table children keys)
  "Return the raw original complete expressions in TABLE that matches CHILDREN and KEYS, where CHILDREN is a list of top-level keys as strings, and KEYS is a list of keys as strings under CHILDREN."
  (loop :for child :in children
        :with cache
        :nconc (loop :for terms :in (construct table child keys)
                     :unless (mem (list-string terms) cache)
                       :collect (loop :for term :in terms
                                      :for v = (convert term)
                                      :when (valid-terms-p term #'base-namespace-p)
                                        :do (pushnew (list-string v) cache :test #'equal)
                                      :collect v))))

(defun* (collect t) (&rest keys)
  "Return the original expressions in TABLE."
  (declare (ignorable keys))
  (let* ((table (atom-table *universe*))
         (children (children table)))
    (mapcar #'list-string (%collect table children keys))))

(defun* (collect-expr t) (spec)
  "Return the original expressions in TABLE."
  (flet ((fn (expr)
           (destructuring-bind (((ns key) &rest _) &rest __)
               (parse-msl expr)
             (declare (ignore _ __))
             (list ns key))))
    (destructuring-bind (source &rest keys)
        (fn spec)
      (declare (ignorable keys))
      (let* ((table (atom-table *universe*))
             (children (if source (list source) (children table))))
        (apply #'values
               (mapcar #'list-string (%collect table children keys)))))))

(defun* (extract-value t) (path)
  "Return the information specified by PATH."
  (labels ((fn (table path)
             (cond ((singlep path)
                    (multiple-value-bind (val existsp)
                        (gethash (car path) table)
                      (when existsp
                        (cond ((hash-table-p val) val)
                              (t (if (listp val) (car val) val))))))
                   ((hash-table-p (gethash (car path) table))
                    (fn (gethash (car path) table) (cdr path)))
                   (t nil))))
    (fn (atom-table *universe*) path)))

(defun* (collect-value t) (spec)
  "Return the information specified by SPEC stored under the = key."
  (destructuring-bind (&rest val)
      (decompose spec)
    (let* ((path (append val '("=")))
           (value (extract-value path)))
      value)))

(defun* (one-form-p t) (terms)
  "Return true if TERMS is in 1-form."
  (when (length-1 terms)
    (destructuring-bind (path params)
        (last* terms)
      (when*
        (= (length path) 2)
        (null params)))))

(defun* (simple-two-form-p t) (terms)
  "Return true if TERMS is in simple 2-form, that is, only the main value is specified."
  (when (length-1 terms)
    (destructuring-bind (path params)
        (last* terms)
      (when*
        (= (length path) 2)
        params))))

(defun* (recall-value t) (path)
  "Return the value specified in PATH."
  (let ((value (append path '("="))))
    (extract-value value)))

(defun* (strip-lead t) (path)
  "Return path PATH without the leading primary namespace and key."
  (cond ((and (= (length path) 4) (prefixedp (cddr path)))
         (cddr path))
        (t path)))

(defun* (recall-expr t) (expr)
  "Return the minimum expression needed to match EXPR with the database."
  (let* ((value (loop :for val :in (parse-msl expr)
                      :unless (null (last* (cdr val)))
                        :collect (car val)))
         (stage (mapcar #'strip-lead value)))
    ;; (cond ((null stage) (collect-expr expr))
    ;;       (t nil))
    stage))

(defun* (decompose t) (table path)
  "Return the values and metadata for PATH found in TABLE."
  (destructuring-bind (key sub-key &optional &rest constraints)
      path
    (declare (ignorable constraints))
    (let* ((stage (loop :with ht = (gethash key table)
                        :for v :in (%%construct ht (list sub-key) nil)
                        :for kv = (cons key v)
                        :when kv :collect (stage (normalize kv))))
           (value (car stage))
           (main (cddr (remove-if #'consp value)))
           (meta (remove-if-not #'consp value)))
      (cons path (cons main meta)))))

(defun* (part t) (value)
  "Conditionally break down STRING into constituents."
  (cond ((and (> (length value) 1)
              (mem (car value) '(#\@ #\:)))
         (list (string* (car value)) (sequence-string (cdr value))))
        (t (list (sequence-string value)))))

(defun* (pack t) (list)
  "Return a new list where the items are parenthesized if they are not already."
  (mapcar #'(lambda (item)
              (cond ((consp item) item)
                    (t (list item))))
          list))

(defun* (split t) (expr)
  "Return EXPR as tokenized s-expression."
  (let* ((value (mapcar #'string* (parse expr (=sexp))))
         ;; (stage (mapcar #'part value))
         )
    value))
