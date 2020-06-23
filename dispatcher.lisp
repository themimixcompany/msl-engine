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

(defun* with-sub-atom-path-p (path)
  "Retun true if PATH contains a sub-atom path and PATH is not a sub-atom path itself."
  (when* (sub-atom-index path) (not (sub-atom-path-p path))))

(defun key-indicator-p (key)
  "Return true if KEY is one of the key indicators for table values."
  (when* (member key +key-indicators+ :test #'equal)))

(defun empty-params-p (params)
  "Return true if PARAMS is considered empty."
  (or (null params)
      (every #'null params)))

(defun find-table (table)
  "Return the table from the universe identified by TABLE."
  (funcall table *universe*))


;;--------------------------------------------------------------------------------------------------
;; readers
;;--------------------------------------------------------------------------------------------------

(defun* read-term (term &optional
                       (atom-table (atom-table *universe*))
                       (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by TERM in SOURCE."
  (block nil
    (destructuring-bind (path &optional &rest _)
        term
      (declare (ignore _))
      (let ((path (if (key-indicator-p (last* path))
                      path
                      (append path '("=")))))
        (labels ((fn (location value)
                   (cond ((null location) value)
                         (t (let ((val (gethash (car location) value)))
                              (if val
                                  (fn (cdr location) val)
                                  (return nil)))))))
          (if (sub-atom-path-p path)
              (fn path sub-atom-table)
              (fn path atom-table)))))))

(defun read-path (path &optional
                       (atom-table (atom-table *universe*))
                       (sub-atom-table (sub-atom-table *universe*)))
  "Return the value specified by PATH in SOURCE."
  (read-term (list path nil) atom-table sub-atom-table))


;;--------------------------------------------------------------------------------------------------
;; writers
;;--------------------------------------------------------------------------------------------------

(defun save-value (term location table value &optional clear-path)
  "Store VALUE using LOCATION as key in TABLE."
  (destructuring-bind (path &optional &rest params)
      term
    (declare (ignorable params))
    (let ((v (if (with-sub-atom-path-p value)
                 (sub-atom-path value)
                 value)))
      (when clear-path (clear-path table)
        (when (mem* location '("/" "[]"))
          (clear-path table path)))
      (setf (gethash (single location) table) v))))

(defun spawn-table (location table)
  "Conditionally return a new table for term writing and use location as key for the new table."
  (if (hash-table-p (gethash (car location) table))
      (gethash (car location) table)
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash (car location) table) ht)
        ht)))

(defun* write-term (term atom-table sub-atom-table &key whole)
  "Return a hash table containing the embedded value tables as specified in TERM."
  (destructuring-bind (path &optional params)
      term
    (let ((opt (with-sub-atom-path-p path))
          (parameters (if whole params (car params))))
      (labels ((fn (location flag atom-tab sub-atom-tab)
                 (cond ((and (null location) flag (null parameters))
                        (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab))

                       ((and (null location) flag parameters)
                        (fn '("=") nil atom-tab sub-atom-tab)
                        (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab))

                       ((and (null location) (not flag) (null parameters))
                        nil)

                       ((and (null location) (not flag) parameters)
                        (fn '("=") flag atom-tab sub-atom-tab))

                       ((and (singlep location) (key-indicator-p (single location)))
                        (save-value term location atom-tab parameters)
                        (when flag
                          (fn (sub-atom-path path) nil sub-atom-tab sub-atom-tab)))

                       (t (fn (cdr location) flag (spawn-table location atom-tab) sub-atom-tab)))))
        (fn path opt atom-table sub-atom-table)
        (read-term term atom-table sub-atom-table)))))

(defun* dispatch (expr &optional (log t))
  "Evaluate EXPR as an MSL expression and store the resulting object in the universe."
  (let ((terms (if (consp expr) expr (parse-msl expr)))
        (atom-tab (find-table #'atom-table))
        (sub-atom-tab (find-table #'sub-atom-table)))
    (flet ((fn (term atom-tab sub-atom-tab)
             (destructuring-bind (path &optional &rest params)
                 term
               (let ((opt (with-sub-atom-path-p path))
                     (values (write-term (list path params) atom-tab sub-atom-tab)))
                 (declare (ignorable opt))
                 (when (consp values)
                   (loop :for value :in values
                         :when (valid-terms-p value)
                         :do (dispatch value)))
                 values))))
      (when terms
        (when (and log (stringp expr))
          (write-log expr))
        (loop :for term :in terms
              :collect (fn term atom-tab sub-atom-tab))))))

(defun* dispatch* (&rest args)
  "Call DISPATCH with logging disabled."
  (apply #'(lambda (arg) (dispatch arg nil)) args))
