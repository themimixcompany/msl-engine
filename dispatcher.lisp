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

(defun sub-atom-index (path)
  "Return true if PATH is a sub-atom path."
  (when (and (consp path)
             (not (uiop:emptyp path)))
    (destructuring-bind (ns &optional &rest body)
        path
      (declare (ignorable body))
      (when (namespacep ns)
        (position-if #'sub-namespace-p path :from-end t)))))

(defun sub-atom-path (path)
  "Return the sub-atom path from PATH."
  (when-let ((index (sub-atom-index path)))
    (subseq path index)))

(defun sub-atom-path-p (path)
  "Return true if PATH starts with a sub-atom path."
  (destructuring-bind (ns &optional &rest _)
      path
    (declare (ignore _))
    (sub-namespace-p ns)))

(defun* has-sub-atom-path-p (path)
  "Retun true if PATH contains a sub-atom path and PATH is not a sub-atom path itself."
  (when* (sub-atom-index path) (not (sub-atom-path-p path))))

(defun key-indicator-p (key)
  "Return true if KEY is one of the key indicators for table values."
  (when* (member key +key-indicators+ :test #'equal)))

(defun empty-params-p (params)
  "Return true if PARAMS is considered empty."
  (or (null params)
      (every #'null params)))

(defun* empty-term-p (term)
  "Return true if TERM is considered empty."
  (destructuring-bind (path &optional &rest params)
      term
    (when*
      (empty-params-p params)
      (not (has-sub-atom-path-p path)))))

(defun* find-table (table)
  "Return the table from the universe identified by TABLE."
  (funcall table *universe*))

(defun* head-term-p (term)
  "Return true if TERM is the main term."
  (destructuring-bind (path value)
      term
    (declare (ignore value))
    (when*
      (length= path 2)
      (base-namespace-p (car path)))))

(defun* regex-term-p (term)
  "Return true if TERM is a regex term."
  (destructuring-bind (path &optional &rest value)
      term
    (declare (ignore value))
    (when*
      (length= path 3)
      (string= (last* path) "/"))))


;;--------------------------------------------------------------------------------------------------
;; readers
;;--------------------------------------------------------------------------------------------------

(defun* empty-key-p (value)
  "Return true if VALUE contains an empty hash table. The second value is true if the path to the table exists."
  (if (hash-table-p value)
      (values (zerop (hash-table-count value))
              t)
      (values nil
              nil)))

(defun* read-term (term &optional
                       (atom-table (atom-table *universe*))
                       (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by TERM in SOURCE."
  (let ((default-key '("="))
        ;;(dummy-value '(""))
        (dummy-value nil))
    (flet ((fn (path table)
             (multiple-value-bind (emptyp existsp)
                 (empty-key-p (gethash* path table))
               (cond ((and (null emptyp) (null existsp))
                      (error "The path ~S in ~S does not exist.~%" path table))
                     ((and (null emptyp) existsp)
                      (gethash* (append path default-key) table))
                     (t dummy-value)))))
      (destructuring-bind (path &optional &rest params)
          term
        (declare (ignorable params))
        (let ((table (if (sub-atom-path-p path) sub-atom-table atom-table)))
          (cond ((key-indicator-p (last* path)) (gethash* path table))
                (t (fn path table))))))))

(defun* read-path (path &optional
                       (atom-table (atom-table *universe*))
                       (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by PATH in SOURCE."
  (read-term (list path nil) atom-table sub-atom-table))


;;--------------------------------------------------------------------------------------------------
;; writers
;;--------------------------------------------------------------------------------------------------
(defun* save-value (term location table value &optional clear-path)
  "Store VALUE using LOCATION as specifier in TABLE."
  (destructuring-bind (path &optional &rest params)
      term
    (declare (ignorable params))
    (let ((v (if (has-sub-atom-path-p value)
                 (sub-atom-path value)
                 value)))
      (when clear-path
        (clear-path table path)
        ;; (when (mem* location '("/" "[]"))
        ;;   (clear-path table path))
        )
      (setf (gethash (single location) table) v))))

(defun spawn-table (path table)
  "Conditionally return a new table for term writing and use path as key for the new table."
  (if (hash-table-p (gethash (car path) table))
      (gethash (car path) table)
      (let ((tab (make-hash-table :test #'equal)))
        (setf (gethash (car path) table) tab)
        tab)))

(defun* write-term (term atom-table sub-atom-table &key whole)
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional params)
      term
    (let ((opt (has-sub-atom-path-p path))
          (parameters (if whole params (car params))))
      (labels ((fn (line flag atom-tab sub-atom-tab)
                 (cond ((and (null line)
                             flag
                             (null parameters))
                        (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab))
                       ((and (null line)
                             flag
                             parameters)
                        (fn '("=") nil atom-tab sub-atom-tab)
                        (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab))
                       ((and (null line)
                             (not flag)
                             (null parameters))
                        nil)
                       ((and (null line)
                             (not flag)
                             parameters)
                        (fn '("=") flag atom-tab sub-atom-tab))
                       ((and (singlep line)
                             (key-indicator-p (single line)))
                        (save-value term line atom-tab parameters)
                        (when flag
                          (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab)))
                       (t (fn (cdr line) flag (spawn-table line atom-tab) sub-atom-tab)))))
        (fn path opt atom-table sub-atom-table)
        (read-term term atom-table sub-atom-table)))))

(defun* find-head (terms)
  "Return the head term from TERMS."
  (find-if #'head-term-p terms))

(defun* find-regex (terms)
  "Return the regex term from TERMS."
  (find-if #'regex-term-p terms))

(defun* terms-has-value-p (terms)
  "Return true if the head in TERMS has a value."
  (when-let ((head (find-head terms)))
    (not (null* (cdr head)))))

(defun* terms-has-regex-p (terms)
  "Return true if TERMS has regex."
  (when* (find-regex terms)))

(defun* head-value (terms)
  "Return the value specified in the head of TERMS."
  (when-let* ((head (find-head terms))
              (value (cadr head)))
    (when (not (null* value))
      value)))

(defun* head-value* (terms)
  "Return the value specified in the head of TERMS, from the store."
  (when-let ((head (find-head terms)))
    (handler-case (read-term head)
      (error (c)
        (declare (ignore c))
        nil))))

(defun* clear-regex (terms)
  "Remove the regex found in TERMS."
  (when-let ((term (find-regex terms)))
    (clear-path (atom-table *universe*) (car term))))

(defun* %dispatch (term &key log force)
  "Evaluate EXPR as an MSL expression and store the resulting object in the universe."
  (if (and (empty-term-p term)
           (not force))
      nil
      (destructuring-bind (path &optional &rest params)
          term
        (let* ((atom-tab (atom-table *universe*))
               (sub-atom-tab (sub-atom-table *universe*))
               (values (write-term (list path params) atom-tab sub-atom-tab)))
          (when (consp values)
            (loop :for value :in values
                  :when (valid-terms-p value)
                  :do (dispatch value :log log :force force)))
          values))))

(defun* process-terms (terms)
  "Do some processing with TERMS, including invoking destructive functions, then return a new value."
  (cond ((and (rmap-and terms
                        #'terms-has-value-p
                        #'terms-has-regex-p)
              (not (null* (head-value* terms)))
              (not (equal (head-value* terms)
                          (head-value terms))))
         (clear-regex terms)
         (remove-if #'regex-term-p terms))
        (t terms)))

(defun* dispatch (expr &key (log t) force)
  "Evaluate EXPR as an MSL expression and store the resulting object in the universe."
  (when-let* ((expressions (if (consp expr) expr (parse-msl expr)))
              (terms (process-terms expressions))
              (value (mapcar #'(lambda (term)
                                 (%dispatch term :log log :force force))
                             terms)))
    (when (and log (not (null* value)) (stringp expr))
      (write-log expr))
    value))

(defun* dispatch* (&rest args)
  "Apply DISPATCH without logging."
  (apply #'dispatch (append args '(:log nil))))

(defun* dispatch! (&rest args)
  "Clear the universe prior to calling DISPATCH*."
  (clear-universe)
  (apply #'dispatch (append args '(:log nil :force nil))))
