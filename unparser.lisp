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
  (∧ (consp path)
     (mem (car path) (uiop:ensure-list keys))))

(defm def-key-predicate (name items)
  "Define a key member predicate."
  `(def ,name (path)
     (key-member-p ',items path)))

(def-key-predicate @p "@")
(def-key-predicate metadatap ":")
(def-key-predicate modsp ("d" "f"))
(def-key-predicate hashp "#")
(def-key-predicate regexp "/")
(def-key-predicate transformp "[]")
(def metamodsp (path) (rmap-or path #'metadatap #'modsp))

(def flatten-one (list)
  "Return a list where items in LIST are conditionally flattened to one level only"
  (reduce (λ (x y)
            (cond ((rmap-or y
                            #'metadatap
                            #'modsp
                            #'termsp)
                   (append x (list y)))
                  (t (append x y))))
          (mapcar #'uiop:ensure-list list)))

(def merge-colons (value)
  "Merge the colons and keys in VALUE."
  (flet* ((fn (args &optional acc)
            (cond
              ((null args)
               (nreverse acc))

              ;; note: evaluate the consequences of this
              ((consp (car args))
               (fn (cdr args)
                   (cons (car args) acc)))

              ((string= (car args) ":")
               (fn (cddr args)
                   (cons (cat (car args) (cadr args)) acc)))

              (t (fn (cdr args)
                     (cons (car args) acc))))))
    (cond ((∧ (consp value) (¬ (termsp value)))
           (fn value))
          (t value))))

(def merge-sections (sections)
  "Conditionally merge the key sequences in SECTIONS."
  (flet ((fn (section)
           (if (rmap-or section #'@p #'metadatap)
               (cons (cat (car section) (cadr section))
                     (cddr section))
               section)))
    (mapcar #'fn sections)))

(def wrap (items)
  "Conditionally listify the items in ITEMS."
  (flet ((fn (item)
           (cond ((∨ (atom item)
                     (∧ (consp item)
                        (¬ (metamodsp item))
                        ;; note: what are the exact consequences of these?
                        (¬ (stringp (car item)))
                        (¬ (consp (car item)))))
                  (list item))
                 (t item))))
    (let ((value (mapcar #'fn items)))
      value)))

;;; note: examine this function
(def stage (sections)
  "Return a new list with preprocessed elements for wrapping and joining."
  (flet* ((fn (args &optional acc)
            (cond
              ((null args)
               (nreverse acc))

              ((modsp (car args))
               (fn (cdr args)
                   (cons (flatten-list (car args)) acc)))

              ((metadatap (car args))
               (fn (cdr args)
                   (cons (flatten-one (wrap (merge-sections (fn (car args)))))
                         acc)))

              (t (fn (cdr args)
                     (cons (car args) acc))))))
    (fn sections)))

(def normalize (list)
  "Return special merging on items of LIST."
  (flet* ((fn (value)
            (cond ((metadatap value)
                   (loop :for v :in (cdr value) :collect (cons (car value) v)))
                  ((hashp value)
                   (apply #'cat (car value) (cadr value)))
                  (t value))))
    (flatten-one (mapcar #'fn list))))

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

(def head (expr)
  "Return the ns and key sequence from EXPR."
  (when-let ((parse (read-expr expr)))
    (caar parse)))

(def head-only-p (path)
  "Return true if PATH is exclusively a head."
  (∧ (length= path 2)
     (path-exists-p path)))

(def terms-base (terms)
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

(defun base-ns-key-sequence (value)
  "Return the key sequence from value."
  (when (∧ (base-ns-p (car value))
           (consp value))
    (subseq value 0 2)))

(def roots (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (when-let* ((tab (gethash key table))
              (entries (or keys (table-keys tab))))
    (loop :for v :in (assemble tab entries)
          :for kv = (cons key v)
          :when kv :collect (normalize kv))))

(def gird (table root)
  "Return a new root from TABLE and ROOT with proper distribution of sub nss."
  (flet ((fn (table root item)
           (roots (gethash* (base-ns-key-sequence root) table)
                  (car item))))
    (mapcar (λ (item)
              (if (∧ (consp item)
                     (sub-ns-p (car item)))
                  (fn table root item)
                  item))
            root)))

(def construct (table key &optional keys)
  "Return the original expressions in TABLE under KEYS, without further processing."
  (let ((result (mapcar (λ (root)
                          (flatten-one (gird table root)))
                        (roots table key keys))))
    (loop :for value :in result
          :collect (flatten-one (wrap (merge-sections (join-items (stage value))))))))

(def join-items (list)
  "Join the the items in LIST that should be together."
  (flet* ((fn (args &optional acc)
            (cond ((null args) (nreverse acc))
                  ((∨ (base-ns-p (car args))
                      (colon-ns-p (car args)))
                   (fn (cddr args)
                       (cons (cat (car args) (cadr args))
                             acc)))
                  ((consp (car args))
                   (fn (cdr args)
                       (cons (fn (car args))
                             acc)))
                  (t (fn (cdr args)
                         (cons (car args) acc))))))
    (fn list)))

(def convert (terms)
  "Return the original expression from TERMS."
  (flet ((fn (v)
           (destructuring-bind (((ns key) &rest _) &rest __)
               v
             (declare (ignore _ __))
             (car (construct (atom-table *universe*) ns (list key))))))
    (cond ((termsp terms #'base-ns-p) (fn terms))
          (t terms))))

(def %collect (table children keys)
  "Return the raw original complete expressions in TABLE that matches CHILDREN and KEYS, where
CHILDREN is a list of top-level keys as strings, and KEYS is a list of keys as strings under
CHILDREN."
  (loop :for child :in children
        :with cache
        :nconc (loop :for terms :in (construct table child keys)
                     :unless (mem (list-string terms) cache)
                     :collect (loop :for term :in terms
                                    :for v = (convert term)
                                    :when (termsp term #'base-ns-p)
                                    :do (pushnew (list-string v) cache :test #'equalp)
                                    :collect v))))

(def collect (&rest keys)
  "Return the original expressions in TABLE. This is mostly a user function to check if the
expressions can be read from the store."
  (declare (ignorable keys))
  (let* ((table (atom-table *universe*))
         (children (children table)))
    (mapcar #'list-string (%collect table children keys))))

(def clear-expr (expr)
  "Remove the expression under EXPR."
  (let ((head (head expr)))
    (when head
      (clear-path (atom-table *universe*) head))))

(def head-only-paths-p (value)
  "Return true if there’s only one path in PATHS and that there’s only a head."
  (when (length= value 1)
    (head-only-p (car value))))

(def has-head-p (sections)
  "Return true if SECTIONS contain a head section."
  (when sections
    (destructuring-bind (head &optional &rest _)
        sections
      (declare (ignore _))
      (when head
        (destructuring-bind (ns key &optional &rest _)
            head
          (declare (ignore _))
          (path-exists-p (list ns key)))))))

(defun strip-head (path)
  "Return path PATH without the leading primary ns and key."
  (let ((length (length path)))
    (cond ((∨ (∧ (length= path 4)
                 (metamodsp (cddr path)))
              (∧ (length> path 2)
                 (base-ns-p (car path))))
           (cddr path))
          (t path))))

(defun string-hash-p (string)
  "Return true if string is a hash."
  (when (∧ (stringp string)
           (¬ (empty-string-p string)))
    (char= (aref string 0) #\#)))

(defun post-sections (sections)
  "Return more surgical operations on the result of sectioning."
  (let* ((metadata (remove-if-not #'metadatap sections))
         (hash (remove-if-not #'string-hash-p sections))
         (hash-value (when hash (list hash)))
         (start (remove-if (λ (section)
                             (∨ (metadatap section)
                                (string-hash-p section)))
                           sections))
         (lead (list (append (car start) (cdr start)))))
    (append lead metadata hash-value)))

(def sections (expr)
  "Return the original expressions from EXPR. EXPR must already be evaluated prior to calling this
function."
  (destructuring-bind (&optional key &rest keys)
      (head expr)
    (when-let* ((table (atom-table *universe*))
                (roots (roots table key keys))
                (gird (gird table (car roots)))
                (flatten-one (flatten-one gird))
                (stage (stage flatten-one)))
      (loop :for item :in stage
            :with limit = (or (position-if #'consp stage)
                              (length stage))
            :for count :from 1 :to limit
            :collect item :into items
            :finally (let ((value (cons items (nthcdr limit stage))))
                       (return (post-sections value)))))))

(def deconstruct (expr &key force)
  "Return the full deconstruct of EXPR. If FORCE is true, dispatch EXPR unconditionally."
  ;; note: this will fail to go if there are errors in EXPR
  (flet* ((fn (args &optional acc)
            (cond
              ((null args)
               (nreverse acc))

              ((termsp (car args))
               (fn (cdr args)
                   (cons (mapcar #'fn (sections (car args)))
                         acc)))

              (t (fn (cdr args)
                     (cons (car args)
                           acc))))))
    ;;(dispatch expr :log nil :force force)
    (mapcar #'fn (sections expr))))

(def deconstruct* (expr &key (force t))
  "Return the full deconstruct of EXPR from a fresh universe. If FORCE is true, dispatch EXPR
unconditionally."
  (with-fresh-universe ()
    (dispatch expr :log nil :force force)
    (deconstruct expr :force force)))

(def active-paths (sections)
  "Return only sections from SECTIONS that contain valid value information."
  (if (∧ (head-only-p (car sections))
         (length> sections 1))
      (cdr sections)
      sections))

(defm def-checker (name)
  "Define a predicate for testing nss."
  (let* ((part-name (hyphenate-intern nil name "part-p"))
         (ns-name (hyphenate-intern nil name "ns-p")))
    `(def ,part-name (part)
       (destructuring-bind (ns &optional &rest _)
           part
         (declare (ignore _))
         (,ns-name ns)))))
(mapply def-checker @ atom sub metadata)

(defm def-spacer (name)
  "Define a helper for inserting spaces."
  (destructuring-bind (_ position)
      (uiop:split-string (string name) :separator '(#\-))
    (declare (ignore _))
    (let ((fn-name (hyphenate-intern nil "insert" position)))
      `(def ,name (list &rest indexes)
         (flet* ((fn (list args)
                   (cond ((null args) list)
                         (t (fn (,fn-name list (car args) " ")
                                (cdr args))))))
           (fn list indexes))))))
(mapply def-spacer space-before space-after)

(def trim-items (items)
  "Remove the extraneous whitespace from the items in ITEMS."
  (mapcar #'trim items))

(def refine-part (part)
  "Conditionally perform additional processing on PART."
  (macrolet ((~m (&body body)
               `(∧ (consp part)
                   ,@body)))
    (cond
      ((~m (@-part-p part) (length> part 2))
       (space-after part 1))

      ((~m (atom-part-p part) (length= part 2))
       (space-after part 0))

      ((~m (atom-part-p part) (length> part 2))
       (space-after part 0 2))

      ((~m (sub-part-p part))
       (list-string (trim-items (merge-colons part))))

      ((~m (metadata-part-p part))
       (space-before part 0 3))

      (t part))))

(def refine-parts (parts)
  "Apply REFINE-PART to PARTS."
  (mapcar #'refine-part parts))

(def reduce-parts (parts)
  "Return a string from running PARTS through filters."
  (let* ((value (refine-parts parts))
         (stage (flatten-one (wrap (merge-sections (stage value))))))
    (list-string* stage)))

(def reduce-expr (expr)
  "Reduce EXPR to the closest approximate original expression, removing comments and other
non-value data."
  (flet* ((fn (args &optional acc)
            (cond
              ((null args)
               (reduce-parts (nreverse acc)))

              ((termsp (car args))
               (fn (cdr args)
                   (cons (fn (car args)) acc)))

              (t (fn (cdr args)
                     (cons (car args) acc))))))
    (fn (deconstruct* expr))))

(def reduce-exprs (exprs)
  "Return a list of values that corresponding to expressions, including terms reduction."
  (mapcar (λ (expr)
            (cond ((termsp expr)
                   (recall-expr (terms-base expr)))
                  (t expr)))
          exprs))

(defun section-match-p (path section)
  "Return true if PATH matches SECTION."
  (when (consp path)
    (search (subseq path 0 2) section :test #'equalp)))

(def reduce-sections (sections &optional paths)
  "Apply REDUCE-EXPRS to sections."
  (if paths
      (loop :for path :in paths
            :nconc (loop :for section :in sections
                         :when (section-match-p path section)
                         :collect (reduce-exprs section)))
      (loop :for section :in sections
            :if (consp section) :collect (reduce-exprs section)
            :else :collect section)))

(def pad-value (value)
  "Add padding information to VALUE."
  (let ((val (trim value)))
    (cat " " val " ")))

(def pad-value-right (value)
  "Add padding information to the right side of VALUE."
  (let ((val (trim value)))
    (cat val " ")))

(def pad-items (items)
  "Pad the items in ITEMS."
  (mapcar (λ (item)
            (cond ((modsp item)
                   (list-string (merge-colons item)))
                  (t item)))
          items))

(def is-mods-p (value)
  "Return true is STRING is a mod."
  (etypecase value
    (string (mem (elt value 0) '(#\/ #\[)))
    (cons (modsp value))
    (t nil)))

(def pad-section (section)
  "Do additiol padding on ITEMS."
  (flet* ((fn (args &optional acc)
            (cond
              ((null args)
               (nreverse acc))

              ;; note: work on this
              ;; ((∧ (is-mods-p (car args))
              ;;     (is-mods-p (cadr args)))
              ;;  (fn (cddr args)
              ;;      (cons (cadr args)
              ;;            (cons " " (cons (car args) acc)))))

              (t (fn (cdr args)
                     (cons (car args) acc))))))
    (fn section)))

(def pad-sections (sections)
  "Add padding information to the items in SECTIONS."
  (flet ((fn (section)
           (destructuring-bind (head &optional &rest body)
               (pad-items (pad-section section))
             (cond ((∧ (base-ns-p (string (elt head 0)))
                       (length> section 1))
                    (cons (pad-value-right head) body))
                   ((∧ (base-ns-p (string (elt head 0)))
                       (length= section 1))
                    (cons head body))
                   (t (cons (pad-value head) body))))))
    (mapcar #'fn sections)))

(def distill (head sections)
  "Return a final, processed string value from SECTIONS."
  (flet ((fn (head sections)
           (if (has-head-p sections)
               sections
               (cons head sections))))
    (when sections
      (let* ((stage (fn head sections))
             (merge-sections (merge-sections stage))
             (wrap (wrap merge-sections))
             (pad-sections (pad-sections wrap))
             (flatten-one (flatten-one pad-sections))
             (list-string (list-string* flatten-one)))
        list-string))))

(def recall-expr (expr &key (dispatch t))
  "Return the matching expression from the store with EXPR."
  (block nil
    ;;(dbg (read-expr expr))

    (when dispatch (dispatch expr :log t :force nil))

    (let ((head (head expr)))
      (when (path-exists-p head)
        (let* ((sections (deconstruct expr))
               (all-paths (deconstruct* expr))
               (active-paths (active-paths (deconstruct* expr))))
          (cond ((head-only-paths-p all-paths)
                 (distill head (reduce-sections sections)))
                (t (distill head (reduce-sections sections active-paths)))))))))

(def recall-expr* (expr)
  "Apply RECALL-EXPR without dispatching."
  (recall-expr expr :dispatch nil))


;;--------------------------------------------------------------------------------------------------
;; recall-value
;;--------------------------------------------------------------------------------------------------

(def reduce-values (values)
  "Return a string concatenation of the items in VALUES."
  (let ((result (mapcar (λ (value)
                          (cond
                            ((exprp value) (recall-value value))
                            ((stringp value) value)
                            (t nil)))
                        values)))
    (join result "")))

(def %extract-value (path)
  "Return the information specified by PATH."
  (flet* ((fn (table path)
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

(def extract-value (path)
  "Return the value specified in PATH."
  (let ((value (append path '("="))))
    (%extract-value value)))

(def car-only (sequence)
  "Return a new sequence with only the first element in SEQUENCE."
  (when (length>= sequence 1)
    (subseq sequence 0 1)))

(def has-metadata-p (path)
  "Return true if PATH contains a metadata subsection."
  (metadatap (strip-head path)))

(def has-mods-p (path)
  "Return true if PATH contains a mods subsection."
  (∨ (modsp (strip-head path))
     (modsp (strip-head (strip-head path)))))

(def has-metamods-p (path)
  "Return true if PATH contains metadata or mods."
  (metamodsp (strip-head path)))

(def has-regex-p (path)
  "Return true if PATH contains a regex subsection."
  (∨ (regexp (strip-head path))
     (regexp (strip-head (strip-head path)))))

(def has-transform-p (path)
  "Return true if PATH contains a transform subsection."
  (transformp (strip-head path)))

(def requests (expr)
  "Return paths from EXPR that are valid requests."
  (when-let ((parse (read-expr expr)))
    (flet ((fn (term)
             (∨ (∧ (length= (car term) 2)
                   (null* (cadr term)))
                (mem (end (car term)) '("/" "[]")))))
      (cond
        ;; top-level expression
        ((length= parse 1)
         (car-only (car parse)))

        ;; no main value, but there is regex
        ((∧ (null* (cdar parse))
            (mem (end (caadr parse)) '("/")))
         (car-only (car parse)))

        ;; no main value, and the rest are either mods or transforms
        ((∧ (null* (cdar parse))
            (every (λ (term)
                     (rmap-or (car term)
                              #'has-mods-p
                              #'has-transform-p))
                   (cdr parse)))
         (car-only (car parse)))

        ;; ;; no main value, there is at least one mod, and there are no metadata
        ;; ((∧ (null* (cdar parse))
        ;;     (find-if #'has-mods-p parse :key #'car)
        ;;     (find-if-not #'has-metadata-p parse :key #'car))
        ;;  (car-only (cadr parse)))

        ;; no main value, there is at least one mod
        ((∧ (null* (cdar parse))
            (some (λ (term)
                    (rmap-and (car term) #'has-mods-p))
                  parse))
         (car-only (cadr parse)))

        ;; remove regexes, transforms, and empty heads
        (t (mapcar #'car (remove-if #'fn parse)))))))

(defun metamods-count (path)
  "Return the number of metamods in PATH."
  (count-if #'has-metamods-p path))

(def %recall-value (expr)
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
      ((∧ (> metamods-count 1)
          (every #'has-metadata-p paths))
       (extract-value head))

      ;; there is more than one metadata path found, but some of them are not metadata paths, like
      ;; (d) and (f)
      ((∧ (> metamods-count 1)
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
  (flet* ((fn (args val)
            (cond ((null args) val)
                  (t (fn (cdr args)
                         (apply-regex-set (car args) val))))))
    (fn regex-sets value)))

(def recall-value (expr &key (dispatch t))
  "Return the value implied by EXPR."
  (block nil
    ;;(dbg (read-expr expr))

    (when dispatch
      (dispatch expr :log t :force t))

    (when-let ((value (%recall-value expr)))
      (let* ((source-path (car (requests expr)))
             (regex-path (ensure-regex-path source-path))
             (regex-sets (regex-path-regexes regex-path)))
        (cond (regex-sets (apply-regex-sets regex-sets value))
              ((termsp value) (recall-value (terms-base value)))
              (t value))))))

(def recall-value* (expr)
  "Apply RECALL-VALUE without dispatching."
  (recall-value expr :dispatch nil))


;;--------------------------------------------------------------------------------------------------
;; entrypoints
;;--------------------------------------------------------------------------------------------------

(def recall (expr &key log)
  "Return the results of expression and value recalls as multiple values"
  (dispatch expr :log log :force nil)
  (values (recall-expr expr :dispatch nil)
          (recall-value expr :dispatch nil)))

(def recall* (expr &key log)
  "Return the results of expression and value recalls as multiple values if the value recall is not
null."
  (dispatch expr :log log :force nil)
  (let ((value (recall-value expr :dispatch nil)))
    (if (null value)
        (values nil
                nil)
        (values (recall-expr expr :dispatch nil)
                value))))
