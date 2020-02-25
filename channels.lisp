;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:nicknames #:s/channels)
  (:export #:make-mx-universe
           #:dump-mx-universe

           ;; #:mtable
           ;; #:wtable
           ;; #:stable
           ;; #:vtable
           ;; #:ctable
           ;; #:atable

           #:make-mx-machine
           #:make-mx-atom

           #:key
           #:table

           #:id
           #:value
           #:metadata
           #:hash
           #:comment

           #:mx-machine
           #:mx-world
           #:mx-stream
           #:mx-view
           #:mx-canon
           #:mx-atom))

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
  ((key :initarg :key
         :initform (uiop:hostname)
         :reader key
         :documentation "The key to designate the mx-machine instance. Defaults to the the hostname.")
   (table :initarg :table
          :initform (make-hash-table :test #'equal)
          :accessor table
          :documentation "The mx-atom table for the mx-machine namespace."))
  (:documentation "The default store to use when there are no other available namespaces. When no names are specified the hostname is used for the instantiation."))

(defclass mx-world ()
  ((key :initarg :key
         :initform ""
         :reader key
         :documentation "The name to designate the mx-world instance.")
   (table :initarg :table
          :initform (make-hash-table :test #'equal)
          :accessor table
          :documentation "The mx-atom table for the mx-world namespace."))
  (:documentation "The structure to designate worlds."))

(defclass mx-stream ()
  ((key :initarg :key
         :initform ""
         :reader key
         :documentation "The name to designate the mx-stream instance.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-stream namespace."))
  (:documentation "The structure to designate streams."))

(defclass mx-view ()
  ((key :initarg :key
         :initform ""
         :reader key
         :documentation "The name to designate the mx-view instance.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The mx-atom table for the mx-view namespace."))
  (:documentation "The structure to designate views."))

(defclass mx-canon ()
  ((key :initarg :key
         :initform ""
         :reader key
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
   (ns :initarg :ns
       :initform nil
       :reader ns
       :documentation "The namespace key of an mx-atom, whether it is m, s, c, v, or @")
   (key :initarg :key
        :initform nil
        :accessor key
        :documentation "The name of an mx-atom.")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The main value of an mx-atom.")
   (metadata :initarg :metadata
             :initform nil
             :accessor metadata
             :documentation "The secondary data of an mx-atom which contains information about the DATA slot.")
   (hash :initarg :hash
             :initform nil
             :accessor hash
             :documentation "The SHA-256 hash of an atom.")
   (comment :initarg :comment
             :initform nil
             :accessor comment
             :documentation "Free form text about an atom."))
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
    (with-slots (id key)
        mx-atom
      (setf id counter)
      (with-slots (atable)
          mx-universe
        (setf (gethash key atable) mx-atom)))))

(defmethod print-object ((mx-atom mx-atom) stream)
  (print-unreadable-object (mx-atom stream :type t)
    (with-slots (ns id key)
        mx-atom
      (format stream "~A ~A ~A" id ns key))))

(defmethod initialize-instance :after ((mx-machine mx-machine) &key mx-universe)
  "Initialize MX-MACHINE A in MX-UNIVERSE."
  (with-slots (key)
      mx-machine
    (with-slots (mtable)
        mx-universe
      (setf (gethash key mtable) mx-machine))))

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

(defun make-mx-machine (&optional key)
  "Return an instance of the mx-machine class."
  (if key
      (make-instance 'mx-machine :key key :mx-universe streams/ethers:*mx-universe*)
      (make-instance 'mx-machine :mx-universe streams/ethers:*mx-universe*)))

(defun make-mx-atom (ns key value metadata &optional hash comment)
  "Return a new mx-atom instance from arguments."
  (make-instance 'mx-atom :ns ns :key key
                          :value value :metadata metadata
                          :hash hash :comment comment
                          :mx-universe streams/ethers:*mx-universe*))
