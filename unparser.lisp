;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:children
           #:merge-heads
           #:combine
           #:compose
           #:construct
           #:collect
           #:flatten-1))

(in-package #:streams/unparser)

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (loop :for k :being :the :hash-key :in table :collect k)))

(defun table-values (table)
  "Return the direct values under TABLE."
  (when (hash-table-p table)
    (loop :for v :being :the :hash-value :in table :collect v)))

(defun children (table &optional object)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p table)
    (let ((keys (table-keys table)))
      (loop :for key :in keys
            :when (hash-table-p (gethash key table))
            :collect (if object (gethash key table) key)))))

(defun colonp (value)
  "Return true if VALUE is the : namespace."
  (string= (car value) ":"))

(defun id-prefixed-p (value)
  "Return true if VALUE is prefixed by certain namespaces."
  (member (car value) '(":" "d" "f") :test #'equal))

(defun merge-heads (value)
  "Return a list where specific namespace ids are merged with its keys."
  (labels ((fn (val)
             (if (id-prefixed-p val)
                 (cond ((colonp val)
                        (cons (marie:cat (first val) (second val))
                              (cddr val)))
                       (t val))
                 val)))
    (cond ((consp value) (fn value))
          (t value))))

(defun combine (value)
  "Return a single-level flattened, combined list from LIST."
  (labels ((fn (val)
             (if (id-prefixed-p val)
                 (cons (car val) (cadr val))
                 val)))
    (cond ((consp value) (fn value))
          (t value))))

(defun flatten-1 (list)
  "Return a flattened list on one level from LIST."
  (let ((value (mapcar #'(lambda (item)
                           (if (consp item) item (list item)))
                       list)))
    (reduce #'(lambda (x y)
                (cond ((id-prefixed-p y) (append x (list y)))
                      ((id-prefixed-p x) (append (list x) y))
                      (t (append x y))))
            value)))

(defun compose (list)
  "Apply additional merging operations to items in LIST."
  (let ((value (mapcar #'combine list)))
    (loop :for v :in value
          :collect (if (and (consp v) (id-prefixed-p v))
                       (cons (car v) (flatten-1 (mapcar #'combine (cdr v))))
                       v))))

(defun accumulate (value acc)
  "Return an an accumulator value suitable for CONSTRUCT."
  (destructuring-bind (v &optional &rest vs)
      value
    (declare (ignorable vs))
    (cond ((string= "=" v) acc)
          (t (cons v acc)))))

(defun construct (key table)
  "Return the original expressions in TABLE under KEY."
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
                            (cons v (accumulate keys acc))))))))
    (marie:when-let* ((ht (gethash key table)))
      (loop :for v :in (fn ht (table-keys ht) nil)
            :for kv = (cons key v)
            :collect (flatten-1 (mapcar #'merge-heads (flatten-1 (compose kv))))))))

(defun collect (&optional (table (atom-table *universe*)))
  "Return the original expressions in TABLE."
  (let ((keys (children table)))
    (loop :for key :in keys :nconc (construct key table))))
