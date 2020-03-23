;;;; etc.lisp

(uiop:define-package #:streams/etc
  (:use #:cl)
  (:export #:dump-universe
           #:dump-object
           #:dump-atom
           #:dump-table
           #:clear-table))

(in-package #:streams/etc)

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun dump-universe ()
  "Dump the contents of the mx-universe."
  (let* ((universe streams/specials:*mx-universe*)
         (slots (slots streams/specials:*mx-universe*))
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
  (marie:when-let* ((obj (gethash key (streams/classes:atom-table streams/specials:*mx-universe*))))
    (dump-object obj)))

(defun dump-table (table)
  "Print information about SOURCE recursively."
  (marie:dump-table* table))

(defun clear-table (table)
  "Clear all the contents of TABLE."
  (clrhash table))
