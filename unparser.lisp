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

(defun* table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (let ((keys (loop :for k :being :the :hash-key :in table :collect k))
          (ex '(":" "#")))
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

(defun key-member-p (keys path)
  "Return true if KEYS is part of PATH."
  (when* (consp path) (mem (car path) (uiop:ensure-list keys))))

(defun* basep (path)
  "Return true if PATH is in the @ key."
  (key-member-p "@" path))

(defun* metadatap (path)
  "Return true if PATH is in the : namespace."
  (key-member-p ":" path))

(defun* modsp (path)
  "Return true if PATH is a datatype or format form."
  (key-member-p '("d" "f") path))

(defun* metamodsp (path)
  "Return true if PATH is in the metadata or mods namespaces."
  (rmap-or path #'metadatap #'modsp))

(defun* hashp (path)
  "Return true if PATH is in the # namespace."
  (key-member-p "#" path))

(defun* transformp (path)
  "Return true if PATH is in the [] namespace."
  (key-member-p "[]" path))

(defun marshall (list)
  "Return a list where non-cons items are made conses."
  (mapcar #'(lambda (item)
              (if (consp item) item (list item)))
          list))

(defun* flatten-1 (list)
  "Return a list where items in LIST are conditionally flattened to one level only"
  (reduce #'(lambda (x y)
              (cond ((or (metadatap y)
                         (modsp y))
                     (append x (list y)))
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
  (labels ((fn (value)
             (cond ((metadatap value)
                    (loop :for v :in (cdr value) :collect (cons (car value) v)))
                   ((hashp value)
                    (apply #'cat (car value) (cadr value)))
                   (t value))))
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

(defun* assemble (table keys &optional acc)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let ((v (gethash (car keys) table)))
    (cond ((null keys) (nreverse acc))
          ((hash-table-p v) (assemble table
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
  (when (and (base-namespace-p (car value))
             (consp value))
    (subseq value 0 2)))

(defun* roots (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let* ((tab (gethash key table))
         (entries (or keys (table-keys tab))))
    (loop :for v :in (assemble tab entries)
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
  (when* (and (length= path 2)
              (head-exists-p path))))

(defun* clear-expr (expr)
  "Remove the expression under EXPR."
  (let ((head (head expr)))
    (when head
      (clear-path (atom-table *universe*) head))))

(defun* solop (value)
  "Return true if VALUE is the only section and that there’s only a head."
  (head-only-p (and (length= value 1) (car value))))

(defun* has-head-p (sections)
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

(defun* string-hash-p (string)
  "Return true if string is a hash."
  (when (and (stringp string)
             (not (empty-string-p string)))
    (char= (aref string 0) #\#)))

(defun* post-sections (sections)
  "Return more surgical operations on the result of sectioning."
  (let* ((metadata (remove-if-not #'metadatap sections))
         (hash (remove-if-not #'string-hash-p sections))
         (start (remove-if #'(lambda (section)
                               (or (metadatap section)
                                   (string-hash-p section)))
                           sections))
         (lead (list (append (car start) (cdr start)))))
    (append lead metadata (list hash))))

(defun* sections (head &key post)
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
            :finally (let ((value (cons items (nthcdr limit stage))))
                       (return (if post
                                   (post-sections value)
                                   value)))))))

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
    (when-let* ((parse (parse-msl expr))
                (strip (strip-heads parse))
                (head (head expr))
                (dispatch (dispatch expr :log nil :force t)))
      (cond ((and (null* dispatch)
                  (null (find-if #'modsp strip)))
             strip)
            (t (sections head :post t))))))

(defun* process (head sections)
  "Return a final, processed string value from SECTIONS."
  (when sections
    (let ((value (if (has-head-p sections)
                     sections
                     (cons head sections))))
      (list-string (flatten-1 (wrap (stage value)))))))

(defun* paths (deconstruct)
  "Return only sections from DECONSTRUCT that contain valid value information."
  (if (and (head-only-p (car deconstruct))
           (> (length deconstruct) 1))
      (cdr deconstruct)
      deconstruct))

(defun section-match-p (path section)
  "Return true if PATH matches SECTION."
  (when (consp path)
    (search (subseq path 0 2) section :test #'equal)))

(defun* recall-expr (expr &key (dispatch t))
  "Return the matching expression from the store with EXPR."
  (when dispatch (dispatch expr :log t :force nil))
  (let ((head (head expr)))
    (when (head-exists-p head)
      (let* ((deconstruct (deconstruct expr))
             (paths (paths deconstruct))
             (sections (sections head :post t)))
        (flet ((fn (paths sections)
                 (loop :for path :in paths
                       :nconc (loop :for section :in sections
                                    :when (section-match-p path section)
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

(defun* car-only (sequence)
  "Return a new sequence with only the first element in SEQUENCE."
  (subseq sequence 0 1))

(defun* requests (expr)
  "Return terms from EXPR that are valid requests."
  (when-let ((parse (parse-msl expr)))
    (flet ((fn (term)
             (or (mem (last* (car term)) '("/" "[]"))
                 (and (length= (car term) 2)
                      (null* (cadr term))))))
      (cond ((length= parse 1)
             (car-only (car parse)))

            ((and (null* (cdar parse))
                  (mem (last* (caadr parse)) '("/")))
             (car-only (car parse)))

            ((and (null* (cdar parse))
                  (every #'(lambda (term)
                             (rmap-or (car term) #'has-mods-p #'has-transform-p))
                         (cdr parse)))
             (car-only (car parse)))

            ((and (null* (cdar parse))
                  (find-if-not #'has-metadata-p parse :key #'car)
                  (find-if #'has-mods-p parse :key #'car))
             (car-only (cadr parse)))

            (t (let ((value (remove-if #'fn parse)))
                 (mapcar #'car value)))))))

(defun* has-metadata-p (path)
  "Return true if PATH contains a metadata subsection."
  (metadatap (strip-head path)))

(defun* has-mods-p (path)
  "Return true if PATH contains a mods subsection."
  (or (modsp (strip-head path))
      (modsp (strip-head (strip-head path)))))

(defun* has-transform-p (path)
  "Return true if PATH contains a transform subsection."
  (transformp (strip-head path)))

(defun* has-metamods-p (path)
  "Return true if PATH is contains metadata or mods."
  (metamodsp (strip-head path)))

(defun* metamods-count (path)
  "Return the number of metamods in PATH."
  (count-if #'has-metamods-p path))

(defun* %recall-value (expr)
  "Return the value specified in EXPR."
  (let* ((paths (requests expr))
         (metamods-count (metamods-count paths))
         (head (head expr)))
    (cond ((solop paths)
           (extract-value (car paths)))
          ((and (> metamods-count 1)
                (every #'has-metadata-p paths))
           (extract-value head))
          ((and (> metamods-count 1)
                (notevery #'has-metadata-p paths))
           (extract-value head))
          ((= metamods-count 1)
           (extract-value (car paths)))
          (t nil))))

(defun ensure-regex-path (path)
  "Return a path with regex from PATH."
  (if (string= (last* path) "/")
      path
      (append path '("/"))))

(defun regex-path-regex (regex-path)
  "Return the regex mod of the implied path in EXPR."
  (when-let ((value (gethash* regex-path (atom-table *universe*))))
    (car value)))

(defun* recall-value (expr &key (dispatch t))
  "Return the value implied by EXPR."
  (when dispatch (dispatch expr :log t :force t))
  (let* ((value (%recall-value expr))
         (parse (parse-msl expr))
         (source-path (car (last* parse)))
         (regex-path (ensure-regex-path source-path))
         (regex (regex-path-regex regex-path)))
    (cond (regex (destructuring-bind (re flag consume)
                     regex
                   (declare (ignorable flag))
                   (cond (consume (values (cl-ppcre:regex-replace re value consume)))
                         (t (values (cl-ppcre:scan-to-strings re value))))))
          (t value))))


;;--------------------------------------------------------------------------------------------------
;; entrypoints
;;--------------------------------------------------------------------------------------------------

(defun* recall (expr &key log)
  "Return the results of expression and value recalls."
  (dispatch expr :log log :force nil)
  (values (recall-expr expr :dispatch nil)
          (recall-value expr :dispatch nil)))

(defun* recall* (expr &key log)
  "Return the results of expression and value recalls."
  (dispatch expr :log log :force nil)
  (let ((value (recall-value expr :dispatch nil)))
    (if (null value)
        (values nil
                nil)
        (values (recall-expr expr :dispatch nil)
                value))))
