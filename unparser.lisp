;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:children
           #:children*
           #:collect
           #:table-keys
           #:table-values
           #:collect*))

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

(defun collect (table key)
  (labels ((fn (tab loc acc)
             (cond ((null loc) (nreverse acc))
                   ((hash-table-p (gethash (car loc) tab))
                    (fn (gethash (car loc) tab)
                        (cdr loc)
                        (cons (children* tab (car loc)) acc)))
                   (t nil))))
    (fn table key nil)))

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (loop :for k :being :the :hash-key :in table :collect k))

(defun table-values (table)
  "Return the direct values under TABLE."
  (loop :for v :being :the :hash-value :in table :collect v))

(defun collect* (table)
  (let ((keys (table-keys table)))
    (loop :for key :in keys
          :when (hash-table-p (gethash key table))
          :nconc (loop :for item :in (table-keys (gethash key table))
                    :collect (list key item)))))

(defun parenthesize (value)
  "Return value as a single parenthesized string."
  (when (consp value)
    (format nil "(~{~A~^ ~})" value)))

(defun build-expr (table path)
  "Return the MSL expression from TABLE specified by PATH."
  table
  path)
