;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:children
           #:table-keys
           #:table-values
           #:combine
           #:construct))

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

(defun combine (value)
  "Return a flattened list from LIST containg the CAR + CADR combinations."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   (t (fn (cadr args)
                          (cons (car args) acc))))))
    (cond ((consp value) (fn value nil))
          (t value))))

(defun construct (key table)
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
                     (t (let ((c (if (string= "=" (car keys))
                                     acc
                                     (cons (car keys) acc))))
                          (fn tab
                              (cdr keys)
                              (cons v c))))))))
    (marie:when-let* ((ht (gethash key table))
                      (value (loop :for v :in (fn ht (table-keys ht) nil)
                                   :collect (cons key v))))
      value)))
