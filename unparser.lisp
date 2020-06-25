;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/dispatcher
        #:marie))

(in-package #:streams/unparser)


;;--------------------------------------------------------------------------------------------------
;; common
;;--------------------------------------------------------------------------------------------------

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (let ((keys (loop :for k :being :the :hash-key :in table :collect k))
          (ex '(":")))
      (if (mem* ex keys)
          (append (remove* ex keys) ex)
          keys))))

(defun* children (table &optional object)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p table)
    (let ((keys (table-keys table)))
      (loop :for key :in keys
            :for value = (gethash key table)
            :when (hash-table-p value)
            :collect (if object value key)))))

(defun basep (path)
  "Return true if PATH is in the @ key."
  (when* (consp path) (mem (car path) '("@"))))

(defun* metadatap (path)
  "Return true if PATH is in the : namespace."
  (when* (consp path) (mem (car path) '(":"))))

(defun* modsp (path)
  "Return true if PATH is a datatype or format form."
  (when* (consp path) (mem (car path) '("d" "f"))))

(defun* metamodsp (path)
  "Return true if PATH is metamods by certain namespaces."
  (rmap-or path #'metadatap #'modsp))

(defun marshall (list)
  "Return a list where non-cons items are made conses."
  (mapcar #'(lambda (item)
              (if (consp item) item (list item)))
          list))

(defun* flatten-1 (list)
  "Return a list where items in LIST are conditionally flattened to one level."
  (reduce #'(lambda (x y)
              (cond ((metadatap y) (append x (list y)))
                    ((modsp y) (append x (list y)))
                    (t (append x y))))
          (marshall list)))

(defun* wrap (list)
  "Return a new list where items in LIST are conditionally listified."
  (mapcar #'(lambda (item)
              (cond ((or (atom item)
                         (and (consp item)
                              (not (metamodsp item))
                              (not (stringp (car item)))))
                     (list item))
                    ((or (basep item)   ;note this
                         (metadatap item))
                     (cons (cat (car item) (cadr item))
                           (cddr item)))
                    (t item)))
          list))

(defun* stage (list)
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

(defun* normalize (list)
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


;;--------------------------------------------------------------------------------------------------
;; recall-expr
;;--------------------------------------------------------------------------------------------------

(defun* assemble (table keys acc)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let ((v (gethash (car keys) table)))
    (cond ((null keys) (nreverse acc))
          ((hash-table-p v)
           (assemble table
             (cdr keys)
             (cons (assemble v
                     (table-keys v)
                     (list (car keys)))
                   acc)))
          (t (assemble table
               (cdr keys)
               (accumulate keys acc v))))))

;;--------------------------------------------------------------------------------------------------
;;; here!

(defun* %construct (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (when-let* ((ht (gethash key table))
              (entries (or keys (table-keys ht))))
    (loop :for v :in (assemble ht entries nil)
          :for kv = (make-head (cons key v))
          :when kv :collect (normalize kv))))

(defun* construct (table key &optional keys)
  "Return the original expressions in TABLE under KEYS."
  (mapcar #'flatten-1 (mapcar #'wrap (mapcar #'stage (%construct table key keys)))))

(defun* base-namespace-key-sequence (value)
  "Return the key sequence from value."
  (when (base-namespace-p (car value))
    (subseq value 0 2)))

(defun* expand-sub-atoms (value)
  "Expand the sub atom lists in VALUE."
  (when-let ((key-sequence (base-namespace-key-sequence value)))
    (mapcar #'(lambda (item)
                (if (sub-namespace-p (car item))
                    ))
            value)))

(defun* %construct-2 (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (flet ((fn (h k ks)
           (let* ((ht (gethash k h))
                  (es (or ks (table-keys ht))))
             (loop :for v :in (assemble ht es nil)
                   :for kv = (cons k v)
                   :when kv :collect (normalize kv)))))
    (loop :for raw-expr :in (fn table key keys)
          :collect (mapcar #'(lambda (v)
                               (if (and (consp v) (sub-namespace-p (car v)))
                                   (fn (gethash* (base-namespace-key-sequence raw-expr)
                                                 table)
                                       (car v)
                                       nil)
                                   v))
                           raw-expr))))

(defun* construct-2 (table key &optional keys)
  "Return the original expressions in TABLE under KEYS."
  (mapcar #'flatten-1 (mapcar #'wrap (mapcar #'stage (%construct-2 table key keys)))))


;;--------------------------------------------------------------------------------------------------

(defun* convert (terms)
  "Return the original expression from TERMS."
  (flet ((fn (v)
           (destructuring-bind (((ns key) &rest _) &rest __)
               v
             (declare (ignore _ __))
             (car (construct (atom-table *universe*) ns (list key))))))
    (cond ((valid-terms-p terms #'base-namespace-p) (fn terms))
          (t terms))))

(defun* %collect (table children keys)
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

(defun* collect (&rest keys)
  "Return the original expressions in TABLE."
  (declare (ignorable keys))
  (let* ((table (atom-table *universe*))
         (children (children table)))
    (mapcar #'list-string (%collect table children keys))))

(defun* collect* (table)
  "Return the original expressions in TABLE."
  (let ((children (children table)))
    (mapcar #'list-string (%collect table children nil))))

(defun* collect-expr (spec)
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

(defun* head (expr)
  "Return the namespace and key sequence from EXPR."
  (when-let ((parse (parse-msl expr)))
    (caar parse)))

(defun* headp (head)
  "Return true if HEAD is a valid path.."
  (gethash* head (atom-table *universe*)))

(defun* head-only-p (path)
  "Return true if PATH is exclusively a head."
  (and (= (length path) 2)
       (headp path)))

(defun* solop (value)
  "Return true if VALUE is the only section and that there’s only a head."
  (head-only-p (and (length-1 value) (car value))))

(defun* with-head-p (sections)
  "Return true if SECTIONS contain a head section."
  (when sections
    (destructuring-bind (head &optional &rest _)
        sections
      (declare (ignore _))
      (when head
        (destructuring-bind (ns key &optional &rest _)
            head
          (declare (ignore _))
          (when* (headp (list ns key))))))))

(defun* strip-head (path)
  "Return path PATH without the leading primary namespace and key."
  (let ((length (length path)))
    (cond ((or (and (= length 4) (metamodsp (cddr path)))
               (and (> length 2) (base-namespace-p (car path))))
           (cddr path))
          (t path))))

(defun* sections (head)
  "Return the original expressions under HEAD according to type."
  (destructuring-bind (key &rest keys)
      head
    (when-let* ((ht (gethash key (atom-table *universe*)))
                (entries (or keys (table-keys ht)))
                (exprs (loop :for v :in (assemble ht entries nil)
                             :for kv = (cons key v)
                             :when kv :collect (normalize kv)))
                (stage (stage (car exprs))))
      (loop :for item :in stage
            :with limit = (or (position-if #'consp stage) (length stage))
            :for count :from 1 :to limit
            :collect item :into items
            :finally (return (cons items (nthcdr limit stage)))))))

(defun* deconstruct (expr)
  "Return the sections of EXPR from a new universe."
  (when-let* ((*universe* (make-universe))
              (head (head expr))
              (dispatch (dispatch* expr)))
    (cond ((null* dispatch)
           (when-let ((parse (parse-msl expr)))
             (mapcar #'(lambda (item) (strip-head (car item)))
                     parse)))
          (t (sections head)))))

(defun process (head sections)
  "Return a final, processed string value from SECTIONS."
  (when sections
    (let ((value (if (with-head-p sections)
                     sections
                     (cons head sections))))
      (list-string (flatten-1 (wrap (stage value)))))))

(defun paths (deconstruct)
  "Return only sections from DECONSTRUCT that contain valid value information."
  (if (head-only-p (car deconstruct))
      (cdr deconstruct)
      deconstruct))

(defun* recall-expr (expr)
  "Return the minimum expression needed to match EXPR from the store."
  (let* ((deconstruct (deconstruct expr))
         (paths (paths deconstruct))
         (head (head expr))
         (sections (sections head)))
    (cond ((solop deconstruct) (process head (sections head)))
          (t (process head
                      (loop :for path :in paths
                            :nconc (loop :for section :in sections
                                         :when (search path section :test #'equal)
                                         :collect section)))))))

(defun* recall-expr* (expr)
  "Dispatch EXPR and perform an expression recall."
  (dispatch* expr)
  (recall-expr expr))


;;--------------------------------------------------------------------------------------------------
;; recall-value
;;--------------------------------------------------------------------------------------------------

(defun* %extract-value (path)
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

(defun* extract-value (path)
  "Return the value specified in PATH."
  (let ((value (append path '("="))))
    (%extract-value value)))

(defun* requests (expr)
  "Return terms from EXPR that are valid requests."
  (when-let ((parse (parse-msl expr)))
    (flet ((fn (item)
             (or (mem (last* (car item)) '("/" "[]" "d" "f"))
                 (and (= (length (car item)) 2)
                      (null (cadr item))))))
      (cond ((length-1 parse) (butlast (car parse)))
            (t (loop :for value :in (remove-if #'fn parse)
                     :for stage = (car value)
                     :collect stage))))))

(defun* with-metadata-p (path)
  "Return true if PATH contains a metadata subsection."
  (metadatap (strip-head path)))

(defun* with-mods-p (path)
  "Return true if PATH contains a mods subsection."
  (modsp (strip-head path)))

(defun* with-metamods-p (path)
  "Return true if PATH is contains metadata or mods."
  (metamodsp (strip-head path)))

(defun* metamods-count (path)
  "Return the number of metamods in PATH."
  (count-if #'with-metamods-p path))

(defun* recall-value (expr)
  "Return the value specified in EXPR."
  (let* ((requests (requests expr))
         (metamods-count (metamods-count requests)))
    (cond ((solop requests) (extract-value (car requests)))
          ((> metamods-count 1) (extract-value (head expr)))
          ((= metamods-count 1) (extract-value (car requests)))
          (t nil))))

(defun* recall-value* (value)
  "Dispatch VALUE and perform a value recall."
  (dispatch* value)
  (recall-value value))
