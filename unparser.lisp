;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:stringify
           #:children
           #:children*
           #:table-keys
           #:table-values
           #:collect
           #:collect*))

(in-package #:streams/unparser)

(defun stringify (value)
  "Return value as a single parenthesized string."
  (when (consp value)
    (format nil "(~{~A~^ ~})" value)))

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
  "Return the result of calling COLLECT on table as list of strings."
  (mapcar #'stringify (collect table)))
