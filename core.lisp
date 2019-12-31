;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:export #:@))

(in-package #:streams/core)

(defun atxm-explore (table)
  "Display the contents of table."
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) table))

(defun atxm-protect-0 (parent key value)
  (let ((current (gethash key parent)))
    (cond ((and (not (hash-table-p current)) (hash-table-p value))
           (setf (gethash 'value value) current)))
    (setf (gethash key parent) value)
    value))

;;; NOTE: Should a mechanism be created to lock a value?
;; (defun atxm-protect (table key value)
;;   "Set VALUE under KEY in TABLE if key is not a table."
;;   (let ((val (atxm-value table key)))
;;     (if (hash-table-p val)
;;         nil
;;         (atxm-value table key value))))

(defparameter *test-table*
  (make-hash-table :test #'equal)
  "A test atxm table.")

(defparameter *test-atxms*
  '((boyfriend (gender m))
    (boyfriend (age 23))
    (favorite-food "creme brulee"))
  "A test atxm tree.")

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

(defun atxm (table key &optional (value nil valuep))
  "Get or set VALUE under KEY in TABLE."
  (if valuep
      (setf (gethash key table) value)
      (multiple-value-bind (val existsp)
          (gethash key table)
        (when existsp
          val))))

(defmacro @ (&rest args)
  "Return a new atxm instance."
  `(let ((atxm (streams/channels:make-atxm ,args)))
    ;; Update atable with the car of atxm name as key and the cdr of atxm value as value
     (when atxm
       atxm)))

;;; (resolve-atxm '(@ foo Foo Bar Baz :bar "bar" :baz 0)) =>
;;; (@ foo "Foo Bar Baz" :bar "bar" :baz 0)
(defun resolve-atxm (atxm)
  "Expand ATXM into its constituent parts."
  (declare (ignorable atxm))
  nil)
