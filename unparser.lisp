;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:children
           #:children*
           #:table-keys
           #:table-values
           #:collect
           #:collect*
           #:construct0
           #:construct))

(in-package #:streams/unparser)

(defun children (table key)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p (gethash key table))
    (loop :for k :being :the :hash-key :of (gethash key table)
          :for entry = (gethash k (gethash key table))
          :when (hash-table-p entry)
          :collect k)))

(defun children* (table key)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p (gethash key table))
    (loop :for k :being :the :hash-key :of (gethash key table)
          :for entry = (gethash k (gethash key table))
          :collect k)))

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (loop :for k :being :the :hash-key :in table :collect k)))

(defun table-values (table)
  "Return the direct values under TABLE."
  (when (hash-table-p table)
    (loop :for v :being :the :hash-value :in table :collect v)))

(defun collect (table)
  "Return the original MSL expressions found in TABLE."
  (let ((keys (table-keys table)))
    (loop :for key :in keys
          :nconc (loop :for item :in (table-keys (gethash key table))
                       :collect (append (list key) (list item)
                                        (collect (gethash item (gethash key table))))))))

(defun collect* (table)
  "Return the result of calling COLLECT on TABLE, as a list of strings."
  (mapcar #'marie:string* (collect table)))

(defun construct0 (table key)
  "Return the original expression in TABLE under KEY."
  (labels ((fn (tab keys acc)
             (let ((v (gethash (car keys) tab)))
               (cond ((null keys) (nreverse acc))
                     ((hash-table-p v)
                      (fn tab
                          (cdr keys)
                          (cons (fn v
                                    (table-keys v)
                                    (list (car keys)))
                                acc)))
                     (t (fn tab
                            (cdr keys)
                            (cons v (cons (car keys) acc))))))))
    (multiple-value-bind (value existsp)
        (gethash key table)
      (declare (ignore value))
      (when existsp
        (fn table (table-keys table) (list key))))))

(defun construct (table)
  "Return the original expression in TABLE under KEY."
  (labels ((fn (tab keys acc)
             (let ((v (gethash (car keys) tab)))
               (cond ((null keys) (nreverse acc))
                     ((hash-table-p v)
                      (fn tab
                          (cdr keys)
                          (cons (fn v
                                    (table-keys v)
                                    (list (car keys)))
                                acc)))
                     (t (fn tab
                            (cdr keys)
                            (cons (car v) (cons (car keys) acc))))))))
    (fn table (table-keys table) nil)))
