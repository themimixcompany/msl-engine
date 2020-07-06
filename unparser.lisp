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
  "Return a list where items in LIST are conditionally flattened to one level only"
  (reduce #'(lambda (x y)
              (cond ((metadatap y) (append x (list y)))
                    ((modsp y) (append x (list y)))
                    (t (append x y))))
          (marshall list)))

(defun* flatten* (list)
  "Return a list where items in LIST are flattened to one level only."
  (reduce #'(lambda (x y) (append x y))
          (marshall list)))

(defun* wrap (list)
  "Return a new list where items in LIST are conditionally listified."
  (mapcar #'(lambda (item)
              (cond ((or (atom item)
                         (and (consp item)
                              (not (metamodsp item))
                              (not (stringp (car item)))))
                     (list item))
                    ((or (basep item)
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

(defun* base-namespace-key-sequence (value)
  "Return the key sequence from value."
  (when (base-namespace-p (car value))
    (subseq value 0 2)))

(defun* roots (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let* ((tab (gethash key table))
         (entries (or keys (table-keys tab))))
    (loop :for v :in (assemble tab entries nil)
          :for kv = (cons key v)
          :when kv :collect (normalize kv))))

(defun* gird (table root)
  "Return a new root from TABLE and ROOT with proper distribution of sub namespaces."
  (flet ((fn (table root item)
           (roots (gethash* (base-namespace-key-sequence root) table)
                  (car item))))
    (mapcar #'(lambda (item)
                (if (and (consp item) (sub-namespace-p (car item)))
                    (fn table root item)
                    item))
            root)))

(defun* construct (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let ((result (mapcar #'(lambda (root)
                            (flatten-1 (gird table root)))
                        (roots table key keys))))
    (loop :for value :in result
          :collect (flatten-1 (wrap (join (stage value)))))))

(defun* join (list)
  "Join the the items in LIST that should be together."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((or (base-namespace-p (car args))
                        (colon-namespace-p (car args)))
                    (fn (cddr args)
                        (cons (cat (car args) (cadr args))
                              acc)))
                   ((consp (car args))
                    (fn (cdr args)
                        (cons (fn (car args) nil)
                              acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (fn list nil)))

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
  "Return the original expressions in TABLE. This is mostly a user function to check if the expressions can be read from the store."
  (declare (ignorable keys))
  (let* ((table (atom-table *universe*))
         (children (children table)))
    (mapcar #'list-string (%collect table children keys))))

(defun* head (expr)
  "Return the namespace and key sequence from EXPR."
  (when-let ((parse (parse-msl expr)))
    (caar parse)))

(defun* head-exists-p (head)
  "Return true if HEAD is a valid path.."
  (gethash* head (atom-table *universe*)))

(defun* head-only-p (path)
  "Return true if PATH is exclusively a head."
  (and (length= path 2)
       (head-exists-p path)))

(defun* clear-expr (expr)
  "Remove the expression under EXPR."
  (let ((head (head expr)))
    (when head
      (clear-path (atom-table *universe*) head))))

(defun* solop (value)
  "Return true if VALUE is the only section and that there’s only a head."
  (head-only-p (and (length= value 1) (car value))))

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
          (when* (head-exists-p (list ns key))))))))

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
    (let* ((table (atom-table *universe*))
           (roots (roots table key keys))
           (stage (stage (flatten-1 (gird table (car roots))))))
      (loop :for item :in stage
            :with limit = (or (position-if #'consp stage) (length stage))
            :for count :from 1 :to limit
            :collect item :into items
            :finally (return (cons items (nthcdr limit stage)))))))

(defun* post-sections (sections)
  "Return more surgical operations on the result of sectioning."
  (let* ((rest (remove-if-not #'metadatap sections))
         (start (remove-if #'metadatap sections))
         (lead (list (append (car start) (cdr start)))))
    (append lead rest)))

(defun* strip-heads (parse)
  "Remove the heads from a parse."
  (remove-if-not #'(lambda (item)
                     (length= item 2))
                 (mapcar #'(lambda (item)
                             (strip-head (car item)))
                         parse)))

(defun* deconstruct (expr)
  "Return the sections of EXPR from a new universe."
  (with-fresh-universe
    (when-let ((head (head expr))
               (dispatch (dispatch expr :log nil :force t)))
      (cond ((null* dispatch)
             (when-let ((parse (parse-msl expr)))
               (strip-heads parse)))
            (t (post-sections (sections head)))))))

(defun* process (head sections)
  "Return a final, processed string value from SECTIONS."
  (when sections
    (let ((value (if (with-head-p sections)
                     sections
                     (cons head sections))))
      (list-string (flatten-1 (wrap (stage value)))))))

(defun* paths (deconstruct)
  "Return only sections from DECONSTRUCT that contain valid value information."
  (if (head-only-p (car deconstruct))
      (cdr deconstruct)
      deconstruct))

(defun* recall-expr (expr)
  "Return the minimum expression needed to match EXPR from the store."
  (dispatch expr :log t)
  (let ((head (head expr)))
    (when (head-exists-p head)
      (let* ((deconstruct (deconstruct expr))
             (paths (paths deconstruct))
             (sections (post-sections (sections head))))
        (flet ((fn (paths sections)
                 (loop :for path :in paths
                       :nconc (loop :for section :in sections
                                    ;; NOTE: match only the head
                                    ;; NOTE: there may be a need to do stronger matching
                                    ;;:when (search path section :test #'equal)
                                    :when (search (subseq path 0 2) section :test #'equal)
                                    :collect section))))
          (cond ((solop deconstruct)
                 (process head sections))
                (t (process head (fn paths sections)))))))))


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
                 (and (length= (car item) 2)
                      (null (cadr item))))))
      (cond ;;((length= parse 1) (butlast (car parse)))

            ((or (length= parse 1)
                 (and (length= parse 2)
                      (null* (cdar parse))
                      (mem (last* (caadr parse)) '("/"))))
             (butlast (car parse)))

            (t (mapcar #'(lambda (value) (car value))
                       (remove-if #'fn parse)))))))

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

(defun* %recall-value (expr)
  "Return the value specified in EXPR."
  (let* ((requests (requests expr))
         (metamods-count (metamods-count requests)))
    (cond ((solop requests) (extract-value (car requests)))
          ((> metamods-count 1) (extract-value (head expr)))
          ((= metamods-count 1) (extract-value (car requests)))
          (t nil))))

(defun* source-path (expr)
  "Return the path implied by EXPR."
  (dispatch expr :log nil :force t)
  (when-let ((value (%recall-value expr))
             (path (car (last* (parse-msl expr)))))
    path))

(defun regex-path (path)
  "Return a path with regex from PATH."
  (if (string= (last* path) "/")
      path
      (append path '("/"))))

(defun* regex-present-p (expr)
  "Return true if a regex mod is present in EXPR."
  (when-let* ((path (source-path expr))
              (regex-path (regex-path path)))
    (when* (gethash* regex-path (atom-table *universe*)))))

(defun* expr-regex (expr)
  "Return the regex mod of the implied path in EXPR."
  (when (regex-present-p expr)
    (let* ((path (source-path expr))
           (regex-path (regex-path path))
           (value (gethash* regex-path (atom-table *universe*))))
      (car value))))

(defun* recall-value (expr)
  "Apply mods if there are any to EXPR."
  (let* ((value (%recall-value expr))
         (regex (expr-regex expr)))
    (cond (regex (destructuring-bind (re flag consume)
                     regex
                   (declare (ignorable flag))
                   (cond (consume (values (cl-ppcre:regex-replace re value consume)))
                         (t (values (cl-ppcre:scan-to-strings re value))))))
          (t value))))


;;--------------------------------------------------------------------------------------------------
;; entrypoints
;;--------------------------------------------------------------------------------------------------

(defun* recall (expr &optional log)
  "Return the results of expression and value recalls."
  (dispatch expr :log log)
  (values (recall-expr expr)
          (recall-value expr)))

(defun* recall* (expr &optional log)
  "Return the results of expression and value recalls."
  (dispatch expr :log log)
  (let ((value (recall-value expr)))
    (if (null value)
        (values nil
                nil)
        (values (recall-expr expr)
                value))))
