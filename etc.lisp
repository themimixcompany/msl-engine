;;;; etc.lisp

(uiop:define-package #:streams/etc
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:slots
           #:dump-universe
           #:dump-object
           #:dump-atom
           #:dump-table
           #:dump-path
           #:tables
           #:clear-table
           #:clear-universe
           #:copy-table
           #:copy-universe))

(in-package #:streams/etc)

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun dump-universe (&optional (universe *mx-universe*))
  "Dump the contents of the mx-universe."
  (let* ((slots (slots universe))
         (string-slots (mapcar #'marie:string-convert slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (format t "UNIVERSE: ~A~%" universe)
    (loop :for table-reader :in table-readers
          :for table = (funcall (intern (marie:string-convert table-reader)
                                        (find-package :streams/classes))
                                universe)
          :do (progn
                (format t "~%~A:~%" table-reader)
                (marie:dump-table* table)))))

(defun dump-object (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (slots object)
        :do (let ((v (funcall slot object)))
              (format t "~S -> ~S~%" slot v)
              (when (hash-table-p v)
                (marie:dump-table v)))))

(defun dump-atom (key)
  "Print information about an atom stored in the universe."
  (marie:when-let* ((obj (gethash key (atom-table *mx-universe*))))
    (dump-object obj)))

(defun dump-table (table)
  "Print information about SOURCE recursively."
  (marie:dump-table* table))

(defun dump-path (path &optional (table (atom-table *mx-universe*)))
  "Dump table information from PATH starting with TABLE."
  (cond ((marie:solop path)
         (multiple-value-bind (val existsp)
             (gethash (marie:stem path) table)
           (when (and (hash-table-p val) existsp)
             (dump-table val))))
        ((hash-table-p (gethash (car path) table))
         (dump-path (cdr path) (gethash (car path) table)))
        (t nil)))

(defun tables (&optional (universe *mx-universe*))
  "Return the list of slots from UNIVERSE that are tables."
  (loop :for slot :in (slots universe)
        :for tab = (funcall slot universe)
        :when (hash-table-p tab)
        :collect tab))

(defun clear-table (table)
  "Clear all the contents of TABLE."
  (clrhash table))

(defun clear-universe (&optional (universe *mx-universe*))
  "Set the current universe to an empty state."
  (loop :for table :in (tables universe) :do (clear-table table)))

(defun copy-table (table &optional params)
  "Create a new hash table from TABLE."
  (let ((ht (make-hash-table :test (hash-table-test table)
                             :rehash-size (hash-table-rehash-size table)
                             :rehash-threshold (hash-table-rehash-threshold table)
                             :size (hash-table-size table))))
    (loop :for key :being :the :hash-key :of table
          :using (hash-value value)
          :do (setf (gethash key ht) value)
          :finally (return ht))))

(defun copy-universe (universe)
  "Return a copy of the mx-universe UNIVERSE, but with a new log date. The
tables are copied using an external function to allow selective table information
copying."
  (with-slots (atom-counter atom-table sub-atom-counter sub-atom-table)
      universe
    (make-instance 'mx-universe
                   :atom-counter atom-counter
                   :atom-table (copy-table atom-table)
                   :sub-atom-counter sub-atom-counter
                   :sub-atom-table (copy-table sub-atom-table))))
