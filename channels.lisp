;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:nicknames #:s/channels)
  (:export #:make-mx-universe
           #:dump-mx-universe
           #:mtable
           #:wtable
           #:stable
           #:vtable
           #:ctable
           #:atable
           #:make-mx-machine
           #:make-mx-atom
           #:name
           #:table
           #:id
           #:data
           #:metadata

           #:mx-machine
           #:mx-world
           #:mx-stream
           #:mx-view
           #:mx-canon))

(in-package #:streams/channels)

(defclass mx-universe ()
  ((mcounter :initarg :mcounter
             :initform streams/ethers:*initial-mcounter*
             :accessor mcounter
             :documentation "The top-level mx-machine counter.")
   (mtable :initarg :mtable
           :initform (make-hash-table :test #'equal)
           :accessor mtable
           :documentation "The top-level colletion of mx-machine, where the key is the name of mx-machine and the value is the instance of that mx-machine.")
   (wcounter :initarg :wcounter
             :initform streams/ethers:*initial-wcounter*
             :accessor wcounter
             :documentation "The top-level mx-world counter.")
   (wtable :initarg :wtable
           :initform (make-hash-table :test #'equal)
           :accessor wtable
           :documentation "The top-level colletion of mx-world, where the key is the name of mx-world and the value is the instance of that mx-world.")
   (scounter :initarg :scounter
             :initform streams/ethers:*initial-scounter*
             :accessor scounter
             :documentation "The top-level mx-stream counter.")
   (stable :initarg :stable
           :initform (make-hash-table :test #'equal)
           :accessor stable
           :documentation "The top-level colletion of mx-stream, where the key is the name of mx-stream and the value is the instance of that mx-stream.")
   (vcounter :initarg :vcounter
             :initform streams/ethers:*initial-vcounter*
             :accessor vcounter
             :documentation "The top-level mx-view counter.")
   (vtable :initarg :vtable
           :initform (make-hash-table :test #'equal)
           :accessor vtable
           :documentation "The top-level colletion of mx-view, where the key is the name of mx-view and the value is the instance of that mx-view.")
   (ccounter :initarg :ccounter
             :initform streams/ethers:*initial-ccounter*
             :accessor ccounter
             :documentation "The top-level mx-canon counter.")
   (ctable :initarg :ctable
           :initform (make-hash-table :test #'equal)
           :accessor ctable
           :documentation "The top-level colletion of mx-canon, where the key is the name of mx-canon and the value is the instance of that mx-canon.")
   (acounter :initarg :acounter
             :initform streams/ethers:*initial-acounter*
             :accessor acounter
             :documentation "The top-level mx-atom counter.")
   (atable :initarg :atable
           :initform (make-hash-table :test #'equal)
           :accessor atable
           :documentation "The top-level colletion of mx-atoms, where the key is the name of the mx-atom an dthe value is the instance of that mx-atom."))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-machine ()
  ((name :initarg :name
         :initform (uiop:hostname)
         :reader name
         :documentation "The name to designate the mx-machine instance. Defaults to the the hostname.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-machine namespace."))
  (:documentation "The default store to use when there are no other available namespaces. When no names are specified the hostname is used for the instantiation."))

(defclass mx-world ()
  ((name :initarg :name
         :initform ""
         :reader name
         :documentation "The name to designate the mx-world instance.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-world namespace."))
  (:documentation "The structure to designate worlds."))

(defclass mx-stream ()
  ((name :initarg :name
         :initform ""
         :reader name
         :documentation "The name to designate the mx-stream instance.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-stream namespace."))
  (:documentation "The structure to designate streams."))

(defclass mx-view ()
  ((name :initarg :name
         :initform ""
         :reader name
         :documentation "The name to designate the mx-view instance.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-view namespace."))
  (:documentation "The structure to designate views."))

(defclass mx-canon ()
  ((name :initarg :name
         :initform ""
         :reader name
         :documentation "The name to designate the mx-canon instance.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-canon namespace."))
  (:documentation "The structure to designate canons."))

(defclass mx-atom ()
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric id of a mx-atom.")
   (category :initarg :category
             :initform nil
             :reader category
             :documentation "The category of an mx-atom, whether it is m, s, c, v, or a")
   (name :initarg :name
         :initform nil
         :reader name
         :documentation "The name of an mx-atom.")
   (data :initarg :data
         :initform nil
         :reader data
         :documentation "The main data of an mx-atom.")
   (metadata :initarg :metadata
             :initform nil
             :reader metadata
             :documentation "The secondary data of an mx-atom which contains information about the DATA slot."))
  (:documentation "The structure to designate atoms."))

(defmacro update-counter (mx-universe accessor)
  "Update the counter in MX-UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,mx-universe))
          (,accessor ,mx-universe)))
(defun update-mcounter (mx-universe)
  "See UPDATE-COUNTER."
  (update-counter mx-universe mcounter))
(defun update-acounter (mx-universe)
  "See UPDATE-COUNTER."
  (update-counter mx-universe acounter))

(defmethod initialize-instance :after ((mx-atom mx-atom) &key mx-universe)
  "Initialize MX-ATOM A in MX-UNIVERSE."
  (let ((counter (update-acounter mx-universe)))
    (with-slots (id name)
        mx-atom
      (setf id counter)
      (with-slots (atable)
          mx-universe
        (setf (gethash name atable) mx-atom)))))

(defmethod print-object ((mx-atom mx-atom) stream)
  (print-unreadable-object (mx-atom stream :type t)
    (with-slots (id)
        mx-atom
      (format stream "~A" id))))

(defmethod initialize-instance :after ((mx-machine mx-machine) &key mx-universe)
  "Initialize MX-MACHINE A in MX-UNIVERSE."
  (with-slots (name)
      mx-machine
    (with-slots (mtable)
        mx-universe
      (setf (gethash name mtable) mx-machine))))

(defmethod print-object ((h hash-table) stream)
  (print-unreadable-object (h stream :type t)
    (let ((test (hash-table-test h))
          (count (hash-table-count h)))
      (format stream "~A ~A" test count))))

(defun make-mx-universe ()
  "Return an instance of the mx-universe class."
  (make-instance 'mx-universe))

(defun dump-mx-universe ()
  "Dump the contents of the mx-universe."
  (let* ((slots (streams/common:slots streams/ethers:*mx-universe*))
         (string-slots (mapcar #'streams/common:string-convert slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (loop :for table :in table-readers
          :do (progn
                (format t "> ~A~%" table)
                (streams/common:dump-table
                 (funcall (read-from-string (mof:cat "STREAMS/CHANNELS:" table))
                          streams/ethers:*mx-universe*))))))

(defun make-mx-machine (&optional name)
  "Return an instance of the mx-machine class."
  (if name
      (make-instance 'mx-machine :name name :mx-universe streams/ethers:*mx-universe*)
      (make-instance 'mx-machine :mx-universe streams/ethers:*mx-universe*)))

(defun make-mx-atom (category name data metadata)
  "Return a new mx-atom instance from arguments."
  (make-instance 'mx-atom :category category :name name
                          :data data :metadata metadata
                          :mx-universe streams/ethers:*mx-universe*))
