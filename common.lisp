;;;; common.lisp

(uiop:define-package #:streams/common
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:marie))

(in-package #:streams/common)

(defun ns-member-p (elem ns-list)
  "Return true if elem is a MEMBER of NS-LIST by CAR."
  (when* (member elem ns-list :key #'car :test #'equal)))

(defun* (base-namespace-p t) (ns)
  "Return true if NS is a base namespace indicator."
  (ns-member-p ns +base-namespace-list+))

(defun* (sub-namespace-p t) (ns)
  "Return true if NS is sub namespace indicator."
  (ns-member-p ns +sub-namespace-list+))

(defun* (namespacep t) (ns)
  "Return true if NS is a namespace indicator."
  (rmap-or ns #'base-namespace-p #'sub-namespace-p))

(defun* (object-slots t) (object)
  "Return the slot names of OBJECT."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun* (slots t) (class)
  "Return the slot names of CLASS."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (find-class class))))

(defun* (dump-object t) (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (object-slots object)
        :do (let ((v (funcall slot object)))
              (format t "~S -> ~S~%" slot v)
              (when (hash-table-p v)
                (dump-table v)))))

(defun* (dump-universe dump t) (&optional (universe *universe*))
  "Dump the contents of the universe."
  (let* ((slots (object-slots universe))
         (string-slots (mapcar #'string* slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (format t "UNIVERSE: ~A~%" universe)
    (loop :for table-reader :in table-readers
          :for table = (funcall (intern (string* table-reader)
                                        (find-package :streams/classes))
                                universe)
          :do (progn
                (format t "~%~A:~%" table-reader)
                (dump-table* table)))))

(defun* (dump-table t) (table)
  "Print information about TABLE recursively."
  (dump-table* table))

(defun* (dump-path t) (table path)
  "Print the information in TABLE specified by PATH."
  (cond ((solop path)
         (multiple-value-bind (val existsp)
             (gethash (stem path) table)
           (when existsp
             (cond ((hash-table-p val) (dump-table val))
                   (t (format t "~S~%" val))))))
        ((hash-table-p (gethash (car path) table))
         (dump-path (gethash (car path) table) (cdr path)))
        (t nil)))

(defun* (clear-table t) (table)
  "Clear all the contents of TABLE."
  (clrhash table))

(defun* (tables t) (&optional (universe *universe*))
  "Return the list of slots from UNIVERSE that are tables."
  (loop :for slot :in (object-slots universe)
        :for tab = (funcall slot universe)
        :when (hash-table-p tab)
        :collect tab))

(defun* (clear-universe clear t) (&optional (universe *universe*))
  "Set the current universe to an empty state."
  (loop :for table :in (tables universe) :do (clear-table table)))

(defun* (copy-universe t) (universe)
  "Return a copy of the universe UNIVERSE, but with a new log date. The
tables are copied using an external function to allow selective table information
copying."
  (with-slots (atom-counter atom-table sub-atom-counter sub-atom-table)
      universe
    (make-instance 'universe
                   :atom-counter atom-counter
                   :atom-table (copy-table atom-table)
                   :sub-atom-counter sub-atom-counter
                   :sub-atom-table (copy-table sub-atom-table))))

(defun* (copy-table t) (table)
  "Create a new hash table from TABLE."
  (let ((ht (make-hash-table :test (hash-table-test table)
                             :rehash-size (hash-table-rehash-size table)
                             :rehash-threshold (hash-table-rehash-threshold table)
                             :size (hash-table-size table))))
    (loop :for key :being :the :hash-key :of table
          :using (hash-value value)
          :do (setf (gethash key ht) value)
          :finally (return ht))))

(defun* (clear-path t) (table path)
  "Remove the specified entry in TABLE that matches PATH."
  (labels ((fn (tab location)
             (cond ((solop location)
                    (remhash (stem location) tab)
                    table)
                   ((hash-table-p (gethash (car location) tab))
                    (fn (gethash (car location) tab) (cdr location)))
                   (t nil))))
    (fn table path)))

(defun clear-other (table key)
  "Remove entries in TABLE that do not match KEY."
  (let ((keys (loop :for k :being :the :hash-key :of table :collect k)))
    (loop :for item :in (remove key keys :test #'equal)
          :do (remhash item table)
          :finally (return table))))

(defun* (filter-path t) (table path)
  "Remove all other table entries in SOURCE that do not match PATH."
  (labels ((fn (tab location)
             (cond ((and (solop location)
                         (hash-table-p (gethash (car location) tab)))
                    (clear-other tab (car location))
                    table)
                   ((hash-table-p (gethash (car location) tab))
                    (clear-other tab (car location))
                    (fn (gethash (car location) tab) (cdr location)))
                   (t nil))))
    (fn table path)))

(defun* (valid-terms-p t) (form &optional (predicate #'namespacep))
  "Return true if FORM is a valid MSL form."
  (flet ((fn (form)
           (destructuring-bind (&optional head &rest _)
               form
             (declare (ignore _))
             (when*
               (consp head)
               (destructuring-bind (value &rest _)
                   head
                 (declare (ignore _))
                 (and (consp value)
                      (funcall predicate (car value))))))))
    (cond ((rmap-or form #'stringp #'numberp) nil)
          (t (fn form)))))
