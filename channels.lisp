;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:nicknames #:s/channels)
  (:export #:make-mx-universe
           #:make-mx-machine
           #:make-mx-atom
           #:dump-mx-atom
           #:name
           #:table
           #:id
           #:data
           #:metadata))

(in-package #:streams/channels)

;;; Note: should an MX-UNIVERSE instance contain all levels?
(defclass mx-universe ()
  ((acounter :initarg :acounter
             :initform streams/globals:*initial-acounter*
             :accessor acounter
             :documentation "The top-level mx-atom counter.")
   (atable :initarg :atable
           :initform (make-hash-table)
           :accessor atable
           :documentation "The top-level colletion of mx-atoms."))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-machine ()
  ((name :initarg :name
         :initform (uiop:hostname)
         :reader name
         :documentation "The name to designate the mx-machine instance. Defaults to the the hostname.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-machine context."))
  (:documentation "The default store to use when there are no other available contexts. When no names are specified the hostname is used for the instantiation."))

(defclass mx-world ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-world context."))
  (:documentation ""))

(defclass mx-stream ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-stream context."))
  (:documentation ""))

(defclass mx-view ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-view context."))
  (:documentation ""))

(defclass mx-canon ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-canon context."))
  (:documentation ""))

(defclass mx-atom ()
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric id of a mx-atom.")
   (category :initarg :category
             :initform nil
             :reader category
             :documentation "The category of an mx-atom, whether it is mx-machine, mx-stream, mx-canon, mx-view, or basic.")
   (data :initarg :data
         :initform nil
         :reader data
         :documentation "The main data of an mx-atom.")
   (metadata :initarg :metadata
             :initform nil
             :reader metadata
             :documentation "The secondary data of an mx-atom which contains information about the DATA slot."))
  (:documentation "The class for holding information about mx-atoms."))

(defmacro spawn-counter (mx-universe accessor)
  "Generate a new counter in MX-UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,mx-universe))
          (,accessor ,mx-universe)))
(defun spawn-acounter (mx-universe)
  "See SPAWN-COUNTER."
  (spawn-counter mx-universe acounter))

(defmethod initialize-instance :after ((a mx-atom) &key mx-universe)
  "Initialize MX-ATOM A in MX-UNIVERSE."
  (let ((counter (spawn-acounter mx-universe)))
    (with-slots (id)
        a
      (setf id counter))))

(defmethod print-object ((a mx-atom) stream)
  (print-unreadable-object (a stream :type t)
    (with-slots (id)
        a
      (format stream "~A" id))))

(defmethod print-object ((h hash-table) stream)
  (print-unreadable-object (h stream :type t)
    (let ((test (hash-table-test h))
          (count (hash-table-count h)))
      (format stream "~A ~A" test count))))

(defun make-mx-universe ()
  "Return an instance of the mx-universe class."
  (make-instance 'mx-universe))

(defun make-mx-machine (&optional name)
  "Return an instance of the mx-machine class."
  (if name
      (make-instance 'mx-machine :name name)
      (make-instance 'mx-machine)))

(defmacro make-mx-machine-2 (&optional name)
  "Return an instance of the mx-machine class."
  `(make-instance 'mx-machine ,@(if name `(:name ,name) ())))

(defun make-mx-atom (category data metadata &key mx-universe)
  "Return a new mx-atom instance from arguments."
  (make-instance 'mx-atom :category category
                          :data data :metadata metadata
                          :mx-universe (or mx-universe streams/globals:*mx-universe*)))