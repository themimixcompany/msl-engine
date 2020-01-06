;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:nicknames #:s/channels)
  (:export #:make-universe
           #:make-atxm
           #:dump-atxm))

(in-package #:streams/channels)

;;; Note: should a universe contain all levels?
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

(defclass machine ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation ""))
  (:documentation ""))

(defclass world ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation ""))
  (:documentation ""))

(defclass strexm ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation ""))
  (:documentation ""))

(defclass view ()
  ((table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation ""))
  (:documentation ""))

(defclass atxm ()
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric id of a atxm.")
   (category :initarg :category
             :initform nil
             :reader category
             :documentation "The category of an atxm, whether it is machine, stream, canon, view, or basic.")
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

;;; Note: should the table in universe be updated to reflect the existence of the new atxm?
(defmethod initialize-instance :after ((a atxm) &key universe)
  "Initialize ATXM A in UNIVERSE."
  (let ((counter (spawn-acounter universe)))
    (with-slots (id)
        a
      (setf id counter))))

(defun make-universe ()
  "Return an instance of the universe."
  (make-instance 'universe))

(defun dump-atxm (atxm)
  "Display the contents of ATXM."
  (loop :for slot :in (streams/common:slots atxm)
        :do (format t "~A: ~S~%" slot (funcall slot atxm))))

(defun atxm-data-p (data)
  "Return true if DATA is a valid atxm."
  (or (and (symbolp data) (not (keywordp data)))
      (stringp data)))

(defun read-atxm (&rest body)
  "Read the atxm data from BODY recursively. The first value retured is the main data while the second value returned is the metadata."
  (labels ((fn (args data)
             (cond ((keywordp (car args)) (values (nreverse data) args))
                   ((null args) (values (nreverse data) nil))
                   ((atxm-data-p (car args))
                    (fn (cdr args) (cons (car args) data)))
                   (t (fn (cddr args) data)))))
    (fn body nil)))

(defun assoc-key (key items)
  "Return the key found in items if key is found."
  (let ((val (assoc key items)))
    (when val
      (car val))))

(defun assoc-value (key items)
  "Return the value found in items if key is found."
  (let ((val (assoc key items)))
    (when val
      (cdr val))))

(defun dotted-pair-p (pair)
  "Return true if LIST is a dotted list."
  (cond ((atom (cdr pair)) t)
        ((listp (cdr pair)) nil)
        (t nil)))

(defun build-pairs (items)
  "Group items into pairs."
  (when (evenp (length items))
    (labels ((fn (items acc)
               (cond ((null items) (nreverse acc))
                     (t (fn (cddr items)
                            (cons (list (first items) (second items))
                                  acc))))))
      (fn items nil))))

(defun build-map (items &key (test #'keywordp))
  "Create key-value mappings from ITEMS."
  (loop :for item :in (build-pairs items)
        :when (funcall test (car item))
        :collect (cons (first item) (second item))))

;;; Note: extend UNIVERSE to apply to context tables
(defun make-atxm (category data metadata &key universe)
  "Return a new atxm instance from arguments."
  (make-instance 'atxm :category category
                       :data data :metadata metadata
                       :universe (or universe streams/globals:*universe*)))

(defun build-atxm (&rest args)
  "Return an atxm instance from args."
  (multiple-value-bind (data metadata)
      (apply #'read-atxm args)
    (let ((head (build-map data :test #'symbolp))
          (body (build-map metadata)))
      (make-atxm 'basic head body))))
