;;;; dispatcher.lisp

(uiop:define-package #:streams/dispatcher
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/logger
        #:marie))

(in-package #:streams/dispatcher)


;;--------------------------------------------------------------------------------------------------
;; general helpers
;;--------------------------------------------------------------------------------------------------

(def sub-atom-index (path)
  "Return true if PATH is a sub-atom path."
  (when (∧ (consp path)
           (¬ (uiop:emptyp path)))
    (destructuring-bind (ns &optional &rest body)
        path
      (declare (ignorable body))
      (when (nsp ns)
        (position-if #'sub-ns-p path :from-end t)))))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (when-let ((index (sub-atom-index path)))
    (subseq path index)))

(def sub-atom-path-p (path)
  "Return true if PATH starts with a sub-atom path."
  (destructuring-bind (ns &optional &rest _)
      path
    (declare (ignore _))
    (sub-ns-p ns)))

(def has-sub-atom-path-p (path)
  "Retun true if PATH contains a sub-atom path and PATH is not a sub-atom path itself."
  (∧ (sub-atom-index path)
     (¬ (sub-atom-path-p path))))

(defun key-indicator-p (key)
  "Return true if KEY is one of the key indicators for table values."
  (∧ (member key +key-indicators+ :test #'equalp)))

(defun empty-params-p (params)
  "Return true if PARAMS is considered empty."
  (∨ (null params)
     (every #'null params)))

(def empty-term-p (term)
  "Return true if TERM is considered empty."
  (destructuring-bind (path &optional &rest params)
      term
    (∧ (empty-params-p params)
       (¬ (has-sub-atom-path-p path)))))

(defun find-table (table)
  "Return the table from the universe identified by TABLE."
  (funcall table *universe*))

(def head-term (terms)
  "Return the head term of TERMS."
  (when (termsp terms)
    (car terms)))

(def term-value (term)
  "Return the value of TERM."
  (destructuring-bind (path &rest value)
      term
    (declare (ignore path))
    value))

(def head-term-p (term)
  "Return true if TERM is the main term."
  (destructuring-bind (path &optional &rest value)
      term
    (declare (ignore value))
    (∧ (length= path 2)
       (base-ns-p (car path)))))

(def metadata-term-p (term)
  "Return true if TERM is a metadata term."
  (destructuring-bind (path &optional &rest value)
      term
    (declare (ignore value))
    (∧ (length= path 4)
       (metadata-ns-p (caddr path)))))

(defun regex-term-p (term)
  "Return true if TERM is a regex term."
  (destructuring-bind (path &optional &rest value)
      term
    (declare (ignore value))
    (∧ (length= path 3)
       (string= (end path) "/"))))

(def atom-exists-p (terms)
  "Return true if the main term in TERMS exists in the store."
  (when (termsp terms)
    (let* ((head (head-term terms))
           (head-term (car head)))
      (path-exists-p head-term))))

(def value-term-p (term)
  "Return true if TERM is a term for holding values. This predicate ignores whether the path exists
in the store or not."
  (∨ (head-term-p term)
     (metadata-term-p term)))

(def recallp (terms)
  "Return true if TERMS is a pure recall."
  (when terms
    (let ((items (loop :for term :in terms
                       :when (value-term-p term)
                       :collect term)))
      (every (λ (item)
               (null* (term-value item)))
             items))))

(def paths-exist-p (terms)
  "Return true if all paths in TERMS exist."
  (every (λ (term)
           (destructuring-bind (path &rest value)
               term
             (declare (ignore value))
             (path-exists-p path)))
         terms))

;;; note: this is a key function
(def valid-recall-p (terms)
  "Return true if TERMS is a valid recall from the store."
  (∧ (recallp terms)
     (atom-exists-p terms)
     (paths-exist-p terms)))

(def metadata-terms (terms)
  "Return all the metadata terms from TERMS."
  (loop for term :in terms
        :when (metadata-term-p term)
        :collect term))

;;; note: this is a key function
(def valid-save-p (terms)
  "Return true if TERMS is a valid save to the store."
  (let ((head-value (term-value (head-term terms))))
    (∨ (not (null* head-value))
       (∧ (null* head-value)
          (atom-exists-p terms)
          (every (λ (term)
                   (not (null* (term-value term))))
                 (metadata-terms terms))))))


;;--------------------------------------------------------------------------------------------------
;; readers
;;--------------------------------------------------------------------------------------------------

(def empty-key-p (value)
  "Return true if VALUE contains an empty hash table. The second value is true if the path to the table exists."
  (if (hash-table-p value)
      (values (zerop (hash-table-count value))
              t)
      (values nil
              nil)))

(def read-term (term &optional
                     (atom-table (atom-table *universe*))
                     (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by TERM in SOURCE."
  (let ((default-key '("="))
        (dummy-value nil)) ;; or '("")
    (flet ((fn (path table)
             (multiple-value-bind (emptyp existsp)
                 (empty-key-p (gethash* path table))
               (cond ((∧ (null emptyp)
                         (null existsp))
                      (error "The path ~S in ~S does not exist.~%" path table))
                     ((∧ (null emptyp)
                         existsp)
                      (gethash* (append path default-key) table))
                     (t dummy-value)))))
      (destructuring-bind (path &optional &rest params)
          term
        (declare (ignorable params))
        (let ((table (if (sub-atom-path-p path) sub-atom-table atom-table)))
          (cond ((key-indicator-p (end path)) (gethash* path table))
                (t (fn path table))))))))

(def read-path (path &optional
                     (atom-table (atom-table *universe*))
                     (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by PATH in SOURCE."
  (read-term (list path nil) atom-table sub-atom-table))


;;--------------------------------------------------------------------------------------------------
;; writers
;;--------------------------------------------------------------------------------------------------

(def save-value (term location table value &optional clear-path)
  "Store VALUE using LOCATION as specifier in TABLE."
  (destructuring-bind (path &optional &rest params)
      term
    (declare (ignorable params))
    (let ((v (if (has-sub-atom-path-p value)
                 (sub-atom-path value)
                 value)))
      (when clear-path (clear-path table path))
      (setf (gethash (single location) table) v))))

(defun spawn-table (path table)
  "Conditionally return a new table for term writing and use path as key for the new table."
  (if (hash-table-p (gethash (car path) table))
      (gethash (car path) table)
      (let ((tab (make-hash-table :test #'equalp)))
        (setf (gethash (car path) table) tab)
        tab)))

(defun write-term (term atom-table sub-atom-table &key whole)
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional params)
      term
    (let ((parameters (if whole params (car params))))
      (flet* ((fn (point flag atom-tab &optional sub-atom-tab)
                (cond
                  ;; point is empty, there are no (d) and (f), and there are no params
                  ((∧ (null point)
                      (null flag)
                      (null parameters))
                   nil)

                  ;; point is empty, there are no (d) and (f), and there are params
                  ((∧ (null point)
                      parameters)
                   (fn '("=") flag atom-tab sub-atom-tab)
                   (when flag
                     (fn (sub-atom-path path) nil sub-atom-tab)))

                  ;; point is empty, there is (d) and (f)
                  ;; write to the sub-atom table
                  ((∧ (null point)
                      flag)
                   (fn (sub-atom-path path) nil sub-atom-tab))

                  ;; save the value, where the single of point, is the table key
                  ((∧ (singlep point)
                      (key-indicator-p (single point)))
                   (save-value term point atom-tab parameters))

                  ;; creating new sub tables
                  (t (fn (cdr point) flag (spawn-table point atom-tab) sub-atom-tab)))))
        (fn path (has-sub-atom-path-p path) atom-table sub-atom-table)
        (read-term term atom-table sub-atom-table)))))

(defun find-head (terms)
  "Return the head term from TERMS."
  (find-if #'head-term-p terms))

(defun terms-has-main-value-p (terms)
  "Return true if the head in TERMS has a value."
  (when-let ((head (find-head terms)))
    (¬ (null* (cdr head)))))

(defun remove-regex (terms)
  "Remove the regex found in TERMS."
  (remove-if #'regex-term-p terms))

(def upcase-key (term)
  "Upcase the key name in TERM"
  (destructuring-bind (path &optional &rest value)
      term
    (cons (progn (setf (nth 1 path) (string-upcase (nth 1 path)))
                 path)
          value)))

(def dispatch-term (term &key force log)
  "Evaluate TERM is the active universe."
  (block nil
    (let ((term (upcase-key term)))
      (cond
        ((∧ (empty-term-p term)
            (¬ force))
         (return nil))
        (t (destructuring-bind (path &optional &rest params)
               term
             (let* ((atom-tab (atom-table *universe*))
                    (sub-atom-tab (sub-atom-table *universe*))
                    (values (write-term (list path params) atom-tab sub-atom-tab)))
               (when (consp values)
                 (loop :for value :in values
                       :do (when (exprp value)
                             (let ((v (dispatch value :log log :force force)))
                               (if (null* v)
                                   (return nil)
                                   v))))
                 values))))))))

(defun term-has-value-p (term)
  "Return true if TERM has a value."
  (destructuring-bind (path &optional &rest value)
      term
    (declare (ignore path))
    (¬ (null* value))))

(def pre-process-terms (terms)
  "Do some processing with TERMS, and the environment, then return a new terms value."
  (let ((%terms (loop :for term :in terms
                      :when (∨ (head-term-p term)
                               (metadata-term-p term))
                      :collect term))
        (keys '("/" "[]" "d" "f")))
    (loop :for key :in keys
          :do (loop :for %term :in %terms
                    :for path = (append (car %term) (list key))
                    :when (∧ (term-has-value-p %term)
                             (gethash* path (atom-table *universe*)))
                    :do (clear-path (atom-table *universe*) path)))
    terms))

(def dispatch (expr &key force log)
  "Evaluate EXPR as an MSL expression and store the resulting object in the universe."
  (block nil
    (when-let* ((parse (read-expr expr))
                (terms (pre-process-terms parse)))
      (let ((value (mapcar (λ (term)
                             (let ((v (dispatch-term term :log log :force force)))
                               (if (null* v)
                                   (return nil)
                                   v)))
                           terms)))
        (when (∧ log (¬ (null* value)) (stringp expr))
          (write-log expr))
        value))))

(def dispatch* (&rest args)
  "Apply DISPATCH without logging."
  (apply #'dispatch (append args '(:log nil))))

(def dispatch! (&rest args)
  "Clear the universe prior to calling DISPATCH*."
  (clear-universe)
  (apply #'dispatch (append args '(:log nil :force nil))))

(def dispatch-terms (terms)
  terms)
