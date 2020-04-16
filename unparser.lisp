;;;; unparser.lisp

(uiop:define-package #:streams/unparser
  (:use #:cl
        #:streams/specials
        #:streams/classes))

(in-package #:streams/unparser)

(defun table-keys (table)
  "Return the direct keys under TABLE."
  (when (hash-table-p table)
    (loop :for k :being :the :hash-key :in table :collect k)))

(defun children (table &optional object)
  "Return all items in TABLE using KEY that are also tables."
  (when (hash-table-p table)
    (let ((keys (table-keys table)))
      (loop :for key :in keys
            :for value = (gethash key table)
            :when (hash-table-p value)
            :collect (if object value key)))))

(defun metadatap (value)
  "Return true if VALUE is the : namespace."
  (marie:when*
    (consp value)
    (marie:mem (car value) '(":"))))

(defun modsp (value)
  "Return true if VALUE is a datatype or format form."
  (marie:when*
    (consp value)
    (marie:mem (car value) '("d" "f"))))

(defun prefixedp (value)
  "Return true if VALUE is prefixed by certain namespaces."
  (marie:rmap-or value #'metadatap #'modsp))

(defun marshall (list)
  "Return a list where non-cons items are made conses."
  (mapcar #'(lambda (item)
              (if (consp item) item (list item)))
          list))

(defun join (list)
  "Return a list where items in LIST are flattened to one level."
  (reduce #'(lambda (x y)
              (cond ((metadatap y) (append x (list y)))
                    ((modsp y) (append x (list y)))
                    (t (append x y))))
          (marshall list)))

(defun wrap (list)
  "Return a new list where items in LIST are conditionally listified."
  (mapcar #'(lambda (item)
              (cond ((or (atom item)
                         (and (consp item)
                              (not (prefixedp item))
                              (not (stringp (car item)))))
                     (list item))
                    ((metadatap item)
                     (cons (marie:cat (car item) (cadr item))
                           (cddr item)))
                    (t item)))
          list))

(defun stage (list)
  "Return a new list from LIST where the items preprocessed for wrapping and joining."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((modsp (car args))
                    (fn (cdr args)
                        (cons (join (car args)) acc)))
                   ((metadatap (car args))
                    (fn (cdr args)
                        (cons (join (wrap (fn (car args) nil)))
                              acc)))
                   (t (fn (cdr args)
                          (cons (car args) acc))))))
    (fn list nil)))

(defun make-regex (exprs)
  "Return a list containing raw regex expressions from VALUE."
  (flet ((fn (expr)
           (destructuring-bind (regex &optional env val)
               expr
             (marie:cat "/" regex "/" (or env "")
                        (if val (marie:cat " " val) "")))))
    (loop :for expr :in exprs :collect (fn expr))))

(defun make-transform (exprs)
  (flet ((fn (expr)
           (marie:cat "[" expr "]")))
    (loop :for expr :in exprs :collect (fn expr))))

(defun normalize (list)
  "Return special merging on items of LIST."
  (labels ((fn (val)
             (cond ((metadatap val) (cons (car val) (cadr val)))
                   (t val))))
    (join (wrap (stage (mapcar #'fn list))))))

(defun attach (list)
  "Return the list (X Y ...) from (X (Y ...)) from LIST."
  (labels ((fn (val)
             (cond ((modsp val) (cons (car val) (cadr val)))
                   (t val))))
    (fn list)))

(defun combine (items)
  "Apply COMBINE on ITEMS."
  (mapcar #'attach items))

(defun compose (items)
  "Apply additional merging operations to items in LIST."
  (labels ((fn (args acc)
             (cond ((null args) (nreverse acc))
                   ((metadatap (car args))
                    (fn (cdr args)
                        (cons (list (caar args)
                                    (combine (cadr (car args))))
                              acc)))
                   (t (fn (cdr args)
                          (cons (attach (car args))
                                acc))))))
    (fn items nil)))

(defun accumulate (keys acc &optional data)
  "Return an accumulator value suitable for CONSTRUCT."
  (flet ((fn (k a d)
           (cond ((marie:mem k '("=")) a)
                 ((marie:mem k '("/")) (cons (make-regex d) a))
                 ((marie:mem k '("[]")) (cons (make-transform d) a))
                 (t (cons k a)))))
    (destructuring-bind (key &optional &rest _)
        keys
      (declare (ignore _))
      (let ((value (fn key acc data)))
        (cond ((marie:mem key '("/" "[]")) value)
              (t (cons data value)))))))

(defun make-head (list)
  "Return a list with custom head merging."
  (destructuring-bind (ns &optional &rest _)
      list
    (declare (ignore _))
    (cond ((string= ns "@")
           (cons (marie:cat ns (cadr list))
                 (cddr list)))
          (t list))))

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
                            (accumulate keys acc v)))))))
    (marie:when-let ((ht (gethash key table)))
      (loop :for v :in (fn ht (table-keys ht) nil)
            :for kv = (make-head (cons key v))
            :collect (normalize (compose kv))))))

(marie:defun* (collect t) (&optional (table (atom-table *universe*)))
  "Return the original expressions in TABLE."
  (labels ((fn (args &optional acc)
             (cond ((null args) (marie:string* (nreverse acc)))
                   ((consp (car args))
                    (fn (cdr args)
                        (cons (fn (car args) nil)
                              acc)))
                   (t (fn (cdr args) (cons (car args) acc))))))
    (let* ((keys (children table))
           (value (loop :for key :in keys :nconc (construct key table))))
      (mapcar #'fn value))))
