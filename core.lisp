;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:export #:make-universe
           #:universe
           #:unit))

(in-package #:streams/core)

(defclass universe ()
  ((ucounter :initarg :ucounter
             :initform streams/globals:*initial-ucounter*
             :accessor ucounter
             :documentation "The top-level unit counter.")
   (utable :initarg :utable
           :initform (make-hash-table)
           :accessor utable
           :documentation "The top-level colletion of units."))
  (:documentation "The top-level data structure for units including information about the current unit counter and the main table."))

(defclass unit ()
  ((uid :initarg :uid
        :initform -1
        :reader uid
        :documentation "The numeric id of a unit.")
   (uvalue :initarg :uvalue
           :initform nil
           :reader uvalue
           :documentation "The value for a unit."))
  (:documentation "The class for holding information about units."))

(defmacro spawn-counter (universe accessor)
  "Generate a new counter in UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,universe))
          (,accessor ,universe)))
(defun spawn-ucounter (universe)
  "See SPAWN-COUNTER."
  (spawn-counter universe ucounter))

(defun make-universe ()
  "Return an instance of the universe."
  (make-instance 'universe))

(defmethod initialize-instance :after ((u unit) &key universe)
  "Initialize unit U in UNIVERSE."
  (let ((counter (spawn-ucounter universe)))
    (with-slots (uid)
        u
      (setf uid counter))))

(defun make-unit (value universe)
  "Return a new unit instance."
  (make-instance 'unit :uvalue value :universe universe))

;;; TODO:
;;; - Add more support for MSL expressions
;;; - Maybe separate the parser and interpreter
