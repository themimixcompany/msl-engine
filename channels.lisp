;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:export #:acounter
           #:atable
           #:aid
           #:avalue
           #:make-universe
           #:make-atxm))

(in-package #:streams/channels)

(defclass universe ()
  ((acounter :initarg :acounter
             :initform streams/globals:*initial-acounter*
             :accessor acounter
             :documentation "The top-level atxm counter.")
   (atable :initarg :atable
           :initform (make-hash-table)
           :accessor atable
           :documentation "The top-level colletion of atxms."))
  (:documentation "The top-level data structure for atxms including information about the current atxm counter and the main table."))

(defclass atxm ()
  ((aid :initarg :aid
        :initform -1
        :reader aid
        :documentation "The numeric id of a atxm.")
   (avalue :initarg :avalue
           :initform nil
           :reader avalue
           :documentation "The value for a atxm."))
  (:documentation "The class for holding information about atxms."))

(defmethod initialize-instance :after ((a atxm) &key universe)
  "Initialize atxm U in UNIVERSE."
  (let ((counter (spawn-acounter universe)))
    (with-slots (aid)
        a
      (setf aid counter))))

(defun make-universe ()
  "Return an instance of the universe."
  (make-instance 'universe))

(defun make-atxm (value &optional (universe streams/globals:*universe*))
  "Return a new atxm instance."
  (make-instance 'atxm :avalue value :universe universe))

(defmacro spawn-counter (universe accessor)
  "Generate a new counter in UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,universe))
          (,accessor ,universe)))
(defun spawn-acounter (universe)
  "See SPAWN-COUNTER."
  (spawn-counter universe acounter))

