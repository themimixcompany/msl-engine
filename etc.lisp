;;;; etc.lisp

(uiop:define-package #:streams/etc
  (:use #:cl)
  (:export #:dump-universe
           #:dump-object
           #:dump-atom))

(in-package #:streams/etc)

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun dump-universe ()
  "Dump the contents of the mx-universe."
  (let* ((slots (slots streams/specials:*mx-universe*))
         (string-slots (mapcar #'marie:string-convert slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (loop :for table :in table-readers
          :do (progn
                (format t "> ~A~%" table)
                (marie:dump-table
                 (funcall (intern (marie:string-convert table)
                                  (find-package :streams/classes))
                          streams/specials:*mx-universe*))))))

(defun dump-object (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (slots object)
        :do (format t "~S => ~S~%" slot (funcall slot object))))

(defun dump-atom (key)
  "Print information about an atom stored in the universe."
  (marie:when-let* ((obj (gethash key (streams/classes:atom-table streams/specials:*mx-universe*)))
                    (table (streams/classes:value obj)))
    (dump-object obj)
    (marie:dump-table table)))
