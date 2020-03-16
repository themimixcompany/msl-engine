;;;; etc.lisp - things that do not belong to the other modules

(uiop:define-package #:streams/etc
  (:use #:cl)
  (:export #:dump-object
           #:dump-universe
           #:dump-msl
           #:dump-metadata))

(in-package #:streams/etc)

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun dump-object (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (slots object)
        :do (format t "~S => ~S~%" slot (funcall slot object))))

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

(defun dump-msl (expr)
  "Print infromation about EXPR as parsed MSL."
  (streams/etc:dump-object (store-msl expr)))

(defun dump-metadata (mx-atom)
  "Print information about the METADATA slot of MX-ATOM."
  (marie:when-let ((metadata (streams/classes:metadata mx-atom)))
    (loop :for m :in metadata :do (streams/etc:dump-object m))))
