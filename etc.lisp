;;;; etc.lisp

(uiop:define-package #:streams/etc
    (:use #:cl)
  (:export #:dump-object))

(in-package #:streams/etc)

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun dump-object (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (slots object)
        :do (format t "~A -> ~S~%" slot (funcall slot object))))

