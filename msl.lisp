;;;; msl.lisp

(uiop:define-package #:streams/msl
    (:use #:cl))

(in-package #:streams/msl)

(defparameter *table* (make-hash-table :test #'equal))

(defun bit-value (table key &optional (value nil valuep))
  "Get or set VALUE under KEY in TABLE."
  (if valuep
      (setf (gethash key table) value)
      (multiple-value-bind (val existsp)
          (gethash key table)
        (when existsp
          val))))

(defun @ (&rest args)
  "Apply BIT-VALUE to ARGS."
  (apply #'bit-value args))

(defun bit-explore (table)
  "Display the contents of table."
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) table))

;;; NOTE: Is it ideal to use 'VALUE as table key?

(defun bit-protect-0 (parent key value)
  (let ((current (gethash key parent)))
    (cond ((and (not (hash-table-p current)) (hash-table-p value))
           (setf (gethash 'value value) current)))
    (setf (gethash key parent) value)
    value))

;;; NOTE: Should a mechanism be created to lock a value?
(defun bit-protect (table key value)
  "Set VALUE under KEY in TABLE if key is not a table."
  (let ((val (bit-value table key)))
    (if (hash-table-p val)
        nil
        (bit-value table key value))))

(defparameter *bits*
  '((boyfriend (gender m))
    (boyfriend (age 23))
    (favorite-food "creme brulee"))
  "A test bit tree.")

(defun aggregate-key (tree key)
  "Find all subtrees in TREE where its CAR is KEY, then return a fresh tree wherein the CAR is KEY and CDR are all matches found."
  (loop :for item :in (copy-tree tree)
        :when (eql (car item) key)
        :nconc (cdr item) :into acc
        :finally (return (if acc (cons key acc) nil))))

(defun aggregate-key-cond (list key &optional acc)
  "Find all subtrees in TREE where its CAR is KEY, then return a fresh tree wherein the CAR is KEY and CDR are all matches found."
  (cond ((and (null list) acc) (cons key (nreverse acc)))
        ((null list) (nreverse acc))
        ((eql key (caar list)) (aggregate-key-cond (cdr list) key (cons (cadar list) acc)))
        (t (aggregate-key-cond (cdr list) key acc))))
