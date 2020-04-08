;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:children
           #:table-keys
           #:table-values
           #:collect
           #:collect*
           #:construct
           #:construct*))

(in-package #:streams/unparser)

(defun children (table key)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p (gethash key table))
    (loop :for k :being :the :hash-key :of (gethash key table)
          :for entry = (gethash k (gethash key table))
          :when (hash-table-p entry)
          :collect k)))

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (loop :for k :being :the :hash-key :in table :collect k)))

(defun table-values (table)
  "Return the direct values under TABLE."
  (when (hash-table-p table)
    (loop :for v :being :the :hash-value :in table :collect v)))

(defun construct (table)
  "Return the original expressions in TABLE."
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

(defun construct* (table key)
  "Return the original expressions in TABLE under KEY."
  (let ((value (loop :for v :in (construct (gethash key table))
                     :collect (cons key v))))
    value))
