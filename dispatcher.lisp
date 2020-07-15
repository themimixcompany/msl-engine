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

(defun read-path (path &optional
                       (atom-table (atom-table *universe*))
                       (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by PATH in SOURCE."
  (read-term (list path nil) atom-table sub-atom-table))


;;--------------------------------------------------------------------------------------------------
;; writers
;;--------------------------------------------------------------------------------------------------

(defun save-value (term line table value &optional clear-path)
  "Store VALUE using LINE as key in TABLE."
  (destructuring-bind (path &optional &rest params)
      term
    (declare (ignorable params))
    (let ((v (if (has-sub-atom-path-p value)
                 (sub-atom-path value)
                 value)))
      (when clear-path (clear-path table path))
      (setf (gethash (single line) table) v))))

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

(defun* dispatch (expr &key (log t) force)
  "Evaluate EXPR as an MSL expression and store the resulting object in the universe."
  (let ((terms (if (consp expr)
                   expr
                   (parse-msl expr))))
    (flet ((fn (term &optional (atom-tab (atom-table *universe*))
                               (sub-atom-tab (sub-atom-table *universe*)))
             (if (and (empty-term-p term)
                      (not force))
                 nil
                 (destructuring-bind (path &optional &rest params)
                     term
                   (let ((values (write-term (list path params) atom-tab sub-atom-tab)))
                     (when (consp values)
                       (loop :for value :in values
                             :when (valid-terms-p value)
                             :do (dispatch value :log log :force force)))
                     values)))))
      (when-let ((value (mapcar #'fn terms)))
        (when (and log
                   (not (null* value))
                   (stringp expr))
          (write-log expr))
        value))))

(defun* dispatch* (&rest args)
  "Apply DISPATCH without logging."
  (apply #'dispatch (append args '(:log nil))))

(defun* dispatch! (&rest args)
  "Clear the universe prior to calling DISPATCH*."
  (clear-universe)
  (apply #'dispatch (append args '(:log nil :force nil))))
