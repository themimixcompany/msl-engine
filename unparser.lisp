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
          (ex '(":" "#")))
      (if (mem* ex keys)
          (append (remove* ex keys) ex)
          keys))))

(defun children (table &optional object)
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

(defun* regexp (path)
  "Return true if PATH is in the / namespace."
  (key-member-p "/" path))

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

(defun* merge-colons (value)
  "Merge the colons and keys in VALUE."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((string= (car args) ":")
                    (fn (cddr args) (cons (cat (car args) (cadr args))
                                          acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (if (and (consp value) (not (termsp value)))
        (fn value nil)
        value)))

(defun* merge-sequences (item)
  "Conditionally merge the key sequences in ITEM."
  (if (rmap-or item #'basep #'metadatap)
      (cons (cat (car item) (cadr item))
            (cddr item))
      item))

;;; note: do not perform merging with c and friends
(defun* merge-all-sequences (items)
  "Conditionally merge the key sequences in ITEMS."
  (let ((value (loop :for item :in items :collect (merge-sequences item))))
    (if (every #'consp value)
        (loop :for val :in value
              :collect (mapcar #'merge-colons val))
        value)))

(defun* wrap (items)
  "Conditionally listify the items in ITEMS."
  (mapcar #'(lambda (item)
              (cond ((or (atom item)
                         (and (consp item)
                              (not (metamodsp item))
                              (not (stringp (car item)))))
                     (list item))
                    (t item)))
          items))

(defun* stage (list)
  "Return a new list with preprocessed elements for wrapping and joining."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((modsp (car args))
                    (fn (cdr args)
                        (cons (flatten-list (car args)) acc)))
                   ((metadatap (car args))
                    (fn (cdr args)
                        (cons (flatten-1 (wrap (merge-all-sequences (fn (car args) nil))))
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

(defun* head (expr)
  "Return the namespace and key sequence from EXPR."
  (when-let ((parse (parse-msl expr)))
    (caar parse)))

(defun* head-only-p (path)
  "Return true if PATH is exclusively a head."
  (when*
    (length= path 2)
    (path-exists-p path)))

(defun make-head (list)
  "Return a list with custom head merging."
  (when (consp (cdr list))
    (destructuring-bind (ns &optional &rest _)
        list
      (declare (ignore _))
      (cond ((string= ns "@")
             (cons (cat ns (cadr list)) (cddr list)))
            (t list)))))

(defun* terms-base (terms)
  "Return the base expression that constitutes TERMS."
  (when (termsp terms)
    (format-parens (reduce #'cat (caar terms)))))


;;--------------------------------------------------------------------------------------------------
;; recall-expr
;;--------------------------------------------------------------------------------------------------

(defun assemble (table keys &optional acc)
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

(defun base-namespace-key-sequence (value)
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
          :collect (flatten-1 (wrap (merge-all-sequences (join-items (stage value))))))))

(defun* join-items (list)
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
    (cond ((termsp terms #'base-namespace-p) (fn terms))
          (t terms))))

(defun* %collect (table children keys)
  "Return the raw original complete expressions in TABLE that matches CHILDREN and KEYS, where
CHILDREN is a list of top-level keys as strings, and KEYS is a list of keys as strings under
CHILDREN."
  (loop :for child :in children
        :with cache
        :nconc (loop :for terms :in (construct table child keys)
                     :unless (mem (list-string terms) cache)
                     :collect (loop :for term :in terms
                                    :for v = (convert term)
                                    :when (termsp term #'base-namespace-p)
                                    :do (pushnew (list-string v) cache :test #'equalp)
                                    :collect v))))

(defun* collect (&rest keys)
  "Return the original expressions in TABLE. This is mostly a user function to check if the
expressions can be read from the store."
  (declare (ignorable keys))
  (let* ((table (atom-table *universe*))
         (children (children table)))
    (mapcar #'list-string (%collect table children keys))))

(defun* clear-expr (expr)
  "Remove the expression under EXPR."
  (let ((head (head expr)))
    (when head
      (clear-path (atom-table *universe*) head))))

(defun* head-only-paths-p (value)
  "Return true if there’s only one path in PATHS and that there’s only a head."
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
          (when* (path-exists-p (list ns key))))))))

(defun strip-head (path)
  "Return path PATH without the leading primary namespace and key."
  (let ((length (length path)))
    (cond ((or (and (= length 4) (metamodsp (cddr path)))
               (and (> length 2) (base-namespace-p (car path))))
           (cddr path))
          (t path))))

(defun string-hash-p (string)
  "Return true if string is a hash."
  (when (and (stringp string)
             (not (empty-string-p string)))
    (char= (aref string 0) #\#)))

(defun post-sections (sections)
  "Return more surgical operations on the result of sectioning."
  (let* ((metadata (remove-if-not #'metadatap sections))
         (hash (remove-if-not #'string-hash-p sections))
         (hash-value (when hash (list hash)))
         (start (remove-if #'(lambda (section)
                               (or (metadatap section)
                                   (string-hash-p section)))
                           sections))
         (lead (list (append (car start) (cdr start)))))
    (append lead metadata hash-value)))

(defun* sections (head &optional post)
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
            (t (sections head))))))

(defun* paths (deconstruct)
  "Return only sections from DECONSTRUCT that contain valid value information."
  (if (and (head-only-p (car deconstruct))
           (length> deconstruct 1))
      (cdr deconstruct)
      deconstruct))

(defun section-match-p (path section)
  "Return true if PATH matches SECTION."
  (when (consp path)
    (search (subseq path 0 2) section :test #'equalp)))

(defun* reduce-exprs (exprs)
  "Return a list of values that corresponding to expressions including terms reduction."
  (mapcar #'(lambda (expr)
              (cond ((stringp expr)
                     expr)
                    ((termsp (car expr))
                     (recall-expr (terms-base (car expr))))
                    ((termsp expr)
                     (recall-expr (terms-base expr)))
                    ((termsp (list expr))
                     (recall-expr (terms-base (list expr))))
                    (t expr)))
          exprs))

(defun* reduce-sections (sections)
  "Apply REDUCE-EXPRS to sections."
  (mapcar #'reduce-exprs sections))

(defun* reduce-matched-sections (paths sections)
  "Apply REDUCE-EXPRS to SECTIONS with PATH."
  (loop :for path :in paths
        :nconc (loop :for section :in sections
                     :when (section-match-p path section)
                     :collect (let ((v (reduce-exprs section)))
                                v))))

(defun* string*-2 (value)
  "Return VALUE to a string."
  (etypecase value
    (number (format nil "~A" value))
    (cons (format nil "(~{~A~^~})" value))
    (string value)
    (t (string value))))

(defun* list-string-2 (list)
  "Return the string version of LIST."
  (labels ((fn (args &optional acc)
             (cond ((null args)
                    (string*-2 (nreverse acc)))
                   ((consp (car args))
                    (fn (cdr args)
                        (cons (fn (car args) nil)
                              acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (fn list)))

(defun* distill (head sections)
  "Return a final, processed string value from SECTIONS."
  (flet ((fn (head sections)
           (if (has-head-p sections)
               sections
               (cons head sections))))
    (when sections
      (let ((value (fn head sections)))
        (list-string-2 (flatten-1 (wrap (merge-all-sequences (stage value)))))))))

(defun* recall-expr (expr &key (dispatch t))
  "Return the matching expression from the store with EXPR."
  (when dispatch (dispatch expr :log t :force nil))
  (let ((head (head expr)))
    (when (path-exists-p head)
      (let* ((deconstruct (deconstruct expr))
             (paths (paths deconstruct))
             (sections (sections head t)))
        (cond ((head-only-paths-p deconstruct)
               (distill head (reduce-sections sections)))
              (t (distill head (reduce-matched-sections paths sections))))))))

(defun* recall-expr* (expr)
  "Apply RECALL-EXPR without dispatching."
  (recall-expr expr :dispatch nil))


;;--------------------------------------------------------------------------------------------------
;; recall-value
;;--------------------------------------------------------------------------------------------------

(defun* reduce-values (values)
  "Return a string concatenation of the items in VALUES."
  (let ((result (mapcar #'(lambda (value)
                       (cond ((stringp value) value)
                             ((termsp value) (recall-value (terms-base value)))
                             (t nil)))
                   values)))
    (join result nil)))

(defun* %extract-value (path)
  "Return the information specified by PATH."
  (labels ((fn (table path)
             (cond ((singlep path)
                    (multiple-value-bind (val existsp)
                        (gethash (car path) table)
                      (when existsp
                        (cond ((hash-table-p val) val)
                              ((listp val)
                               (reduce-values val))
                              (t val)))))
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
  (when (length>= sequence 1)
    (subseq sequence 0 1)))

(defun* has-metadata-p (path)
  "Return true if PATH contains a metadata subsection."
  (metadatap (strip-head path)))

(defun* has-mods-p (path)
  "Return true if PATH contains a mods subsection."
  (or (modsp (strip-head path))
      (modsp (strip-head (strip-head path)))))

(defun* has-metamods-p (path)
  "Return true if PATH contains metadata or mods."
  (metamodsp (strip-head path)))

(defun* has-regex-p (path)
  "Return true if PATH contains a regex subsection."
  (or (regexp (strip-head path))
      (regexp (strip-head (strip-head path)))))

(defun* has-transform-p (path)
  "Return true if PATH contains a transform subsection."
  (transformp (strip-head path)))

(defun* requests (expr)
  "Return paths from EXPR that are valid requests."
  (when-let ((parse (parse-msl expr)))
    (flet ((fn (term)
             (or (and (length= (car term) 2)
                      (null* (cadr term)))
                 (mem (last* (car term)) '("/" "[]")))))
      (cond
        ;; top-level expression
        ((length= parse 1)
         (car-only (car parse)))

        ;; no main value, but there is regex
        ((and (null* (cdar parse))
              (mem (last* (caadr parse)) '("/")))
         (car-only (car parse)))

        ;; no main value, and the rest are either mods or transforms
        ((and (null* (cdar parse))
              (every #'(lambda (term)
                         (rmap-or (car term)
                                  #'has-mods-p
                                  #'has-transform-p))
                     (cdr parse)))
         (car-only (car parse)))

        ;; ;; no main value, there is at least one mod, and there are no metadata
        ;; ((and (null* (cdar parse))
        ;;       (find-if #'has-mods-p parse :key #'car)
        ;;       (find-if-not #'has-metadata-p parse :key #'car))
        ;;  (car-only (cadr parse)))

        ;; no main value, there is at least one mod
        ((and (null* (cdar parse))
              (some #'(lambda (term)
                        (rmap-and (car term) #'has-mods-p))
                    parse))
         (car-only (cadr parse)))

        ;; remove regexes, transforms, and empty heads
        (t (mapcar #'car (remove-if #'fn parse)))))))

(defun metamods-count (path)
  "Return the number of metamods in PATH."
  (count-if #'has-metamods-p path))

(defun* %recall-value (expr)
  "Return the value specified in EXPR."
  (let* ((paths (requests expr))
         (metamods-count (metamods-count paths))
         (head (head expr)))
    (cond
      ;; only the the base path
      ((head-only-paths-p paths)
       (extract-value (car paths)))

      ;; base path is not found, but a single metadata path is found
      ((= metamods-count 1)
       (extract-value (car paths)))

      ;; there is more than one metadata path found, and all of them are metadata paths
      ((and (> metamods-count 1)
            (every #'has-metadata-p paths))
       (extract-value head))

      ;; there is more than one metadata path found, but some of them are not metadata paths, like
      ;; (d) and (f)
      ((and (> metamods-count 1)
            (notevery #'has-metadata-p paths))
       (extract-value head))

      ;; there are no matching criteria
      (t nil))))

(defun regex-path-regexes (regex-path)
  "Return the regex mods of the implied path in EXPR."
  (when-let ((value (gethash* regex-path (atom-table *universe*))))
    value))

(defun apply-regex-set (regex-set value)
  "Apply the regexes from REGEX-SET to VALUE."
  (destructuring-bind (re flags consume)
      regex-set
    (let* ((flags-list (when flags (loop :for c :across flags :collect c)))
           (replace-fn (if (mem #\g flags-list)
                           #'cl-ppcre:regex-replace-all
                           #'cl-ppcre:regex-replace))
           (regex (if (mem #\i flags-list)
                      (cat "(?i)" re)
                      re)))
      (cond (consume (funcall replace-fn regex value consume))
            (t (cl-ppcre:scan-to-strings regex value))))))

(defun apply-regex-sets (regex-sets value)
  "Apply the regex sets from REGEX-SETS with value as the starting point."
  (labels ((fn (args val)
             (cond ((null args) val)
                   (t (fn (cdr args)
                          (apply-regex-set (car args) val))))))
    (fn regex-sets value)))

(defun* recall-value (expr &key (dispatch t))
  "Return the value implied by EXPR."
  (when dispatch (dispatch expr :log t :force t))
  (let* ((value (string-left-trim '(#\Space #\Tab #\Newline) (%recall-value expr)))
         (source-path (car (requests expr)))
         (regex-path (ensure-regex-path source-path))
         (regex-sets (regex-path-regexes regex-path)))
    (cond (regex-sets (apply-regex-sets regex-sets value))
          ((termsp value) (recall-value (terms-base value)))
          (t value))))

(defun* recall-value* (expr)
  "Apply RECALL-VALUE without dispatching."
  (recall-value expr :dispatch nil))


;;--------------------------------------------------------------------------------------------------
;; entrypoints
;;--------------------------------------------------------------------------------------------------

(defun* recall (expr &key log)
  "Return the results of expression and value recalls as multiple values"
  (dispatch expr :log log :force nil)
  (values (recall-expr expr :dispatch nil)
          (recall-value expr :dispatch nil)))

(defun* recall* (expr &key log)
  "Return the results of expression and value recalls as multiple values if the value recall in not null."
  (dispatch expr :log log :force nil)
  (let ((value (recall-value expr :dispatch nil)))
    (if (null value)
        (values nil
                nil)
        (values (recall-expr expr :dispatch nil)
                value))))
