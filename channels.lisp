;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:nicknames #:s/channels)
  (:export #:make-universe
           #:make-atxm
           #:dump-atxm))

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
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric id of a atxm.")
   (data :initarg :data
         :initform nil
         :reader data
         :documentation "The main data of an atxm.")
   (metadata :initarg :metadata
             :initform nil
             :reader metadata
             :documentation "The secondary data of an atxm which contains information about the DATA slot."))
  (:documentation "The class for holding information about atxms."))

(defmacro spawn-counter (universe accessor)
  "Generate a new counter in UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,universe))
          (,accessor ,universe)))
(defun spawn-acounter (universe)
  "See SPAWN-COUNTER."
  (spawn-counter universe acounter))

(defmethod initialize-instance :after ((a atxm) &key universe)
  "Initialize ATXM A in UNIVERSE."
  (let ((counter (spawn-acounter universe)))
    (with-slots (id)
        a
      (setf id counter))))

(defun make-universe ()
  "Return an instance of the universe."
  (make-instance 'universe))

(defun make-atxm (&rest data)
  "Return a new atxm instance from DATA."
  (let ((properties (streams/common:build-properties data)))
    (when properties
      (make-instance 'atxm :data properties :universe streams/globals:*universe*))))

(defun build-atxm (&rest data)
  "Instantiate an atxm and set the global symbol value."
  (let* ((atxm (apply #'make-atxm data))
         (name (getf (data atxm) :primary-key)))
    (handler-bind ((unbound-variable #'(lambda (c)
                                         (declare (ignore c))
                                         (use-value nil))))
      (unless (symbol-value name)
        (setf (symbol-value name) (getf (data atxm) :primary-value))
        atxm))))

(defun dump-atxm (atxm)
  "Display the contents of ATXM."
  (loop :for slot :in (streams/common:slots atxm)
        :do (format t "~A: ~S~%" slot (funcall slot atxm))))
