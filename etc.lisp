;;;; etc.lisp

(uiop:define-package #:streams/etc
  (:use #:cl
        #:streams/specials
        #:streams/classes))

(in-package #:streams/etc)

(marie:defun* (dump-object t) (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (slots object)
        :do (let ((v (funcall slot object)))
              (format t "~S -> ~S~%" slot v)
              (when (hash-table-p v)
                (marie:dump-table v)))))

(marie:defun* (slots t) (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(marie:defun* (dump-universe dump t) (&optional (universe *universe*))
  "Dump the contents of the universe."
  (let* ((slots (slots universe))
         (string-slots (mapcar #'marie:string* slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (format t "UNIVERSE: ~A~%" universe)
    (loop :for table-reader :in table-readers
          :for table = (funcall (intern (marie:string* table-reader)
                                        (find-package :streams/classes))
                                universe)
          :do (progn
                (format t "~%~A:~%" table-reader)
                (marie:dump-table* table)))))

(marie:defun* (dump-table t) (table)
  "Print information about TABLE recursively."
  (marie:dump-table* table))

(marie:defun* (dump-path t) (table path)
  "Print the information in TABLE specified by PATH."
  (cond ((marie:solop path)
         (multiple-value-bind (val existsp)
             (gethash (marie:stem path) table)
           (when existsp
             (cond ((hash-table-p val) (dump-table val))
                   (t (format t "~S~%" val))))))
        ((hash-table-p (gethash (car path) table))
         (dump-path (gethash (car path) table) (cdr path)))
        (t nil)))

(marie:defun* (clear-table t) (table)
  "Clear all the contents of TABLE."
  (clrhash table))

(marie:defun* (tables t) (&optional (universe *universe*))
  "Return the list of slots from UNIVERSE that are tables."
  (loop :for slot :in (slots universe)
        :for tab = (funcall slot universe)
        :when (hash-table-p tab)
        :collect tab))

(marie:defun* (clear-universe clear t) (&optional (universe *universe*))
  "Set the current universe to an empty state."
  (loop :for table :in (tables universe) :do (clear-table table)))

(marie:defun* (copy-universe t) (universe)
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

(marie:defun* (copy-table t) (table)
  "Create a new hash table from TABLE."
  (let ((ht (make-hash-table :test (hash-table-test table)
                             :rehash-size (hash-table-rehash-size table)
                             :rehash-threshold (hash-table-rehash-threshold table)
                             :size (hash-table-size table))))
    (loop :for key :being :the :hash-key :of table
          :using (hash-value value)
          :do (setf (gethash key ht) value)
          :finally (return ht))))

(marie:defun* (clear-path t) (table path)
  "Remove the specified entry in TABLE that matches PATH."
  (labels ((fn (tab location)
             (cond ((marie:solop location)
                    (remhash (marie:stem location) tab)
                    table)
                   ((hash-table-p (gethash (car location) tab))
                    (fn (gethash (car location) tab) (cdr location)))
                   (t nil))))
    (fn table path)))

(marie:defun* (clear-other t) (table key)
  "Remove entries in TABLE that do not match KEY."
  (let ((keys (loop :for k :being :the :hash-key :of table :collect k)))
    (loop :for item :in (remove key keys :test #'equal)
          :do (remhash item table)
          :finally (return table))))

(marie:defun* (filter-path t) (table path)
  "Remove all other table entries in SOURCE that do not match PATH."
  (labels ((fn (tab location)
               (cond ((and (marie:solop location)
                           (hash-table-p (gethash (car location) tab)))
                      (clear-other tab (car location))
                      table)
                     ((hash-table-p (gethash (car location) tab))
                      (clear-other tab (car location))
                      (fn (gethash (car location) tab) (cdr location)))
                     (t nil))))
    (fn table path)))
