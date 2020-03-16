;;;; classes.lisp

(uiop:define-package #:streams/classes
  (:use #:cl)
  (:export #:mx-universe
           #:machine-table
           #:machine-counter
           #:world-table
           #:world-counter
           #:stream-table
           #:stream-counter
           #:view-table
           #:view-counter
           #:canon-table
           #:canon-counter
           #:atom-table
           #:atom-counter

           #:mx-atom
           #:mx-atom-data
           #:mx-atom-metadata

           #:id
           #:ns
           #:key
           #:value
           #:mods
           #:metadata
           #:hash
           #:comment
           #:canonizedp

           #:table-name
           #:make-mx-atom-data
           #:make-mx-atom-metadata
           #:make-mx-atom-modsdata))

(in-package #:streams/classes)

(defclass mx-universe ()
  ((machine-counter :initarg :machine-counter
                    :initform streams/specials:*initial-machine-counter*
                    :accessor machine-counter)
   (machine-table :initarg :machine-table
                  :initform (make-hash-table :test #'equal)
                  :accessor machine-table)
   (world-counter :initarg :world-counter
                  :initform streams/specials:*initial-world-counter*
                  :accessor world-counter)
   (world-table :initarg :world-table
                :initform (make-hash-table :test #'equal)
                :accessor world-table)
   (stream-counter :initarg :stream-counter
                   :initform streams/specials:*initial-stream-counter*
                   :accessor stream-counter)
   (stream-table :initarg :stream-table
                 :initform (make-hash-table :test #'equal)
                 :accessor stream-table)
   (view-counter :initarg :view-counter
                 :initform streams/specials:*initial-view-counter*
                 :accessor view-counter)
   (view-table :initarg :view-table
               :initform (make-hash-table :test #'equal)
               :accessor view-table)
   (canon-counter :initarg :canon-counter
                  :initform streams/specials:*initial-canon-counter*
                  :accessor canon-counter)
   (canon-table :initarg :canon-table
                :initform (make-hash-table :test #'equal)
                :accessor canon-table)
   (atom-counter :initarg :atom-counter
                 :initform streams/specials:*initial-atom-counter*
                 :accessor atom-counter)
   (atom-table :initarg :atom-table
               :initform (make-hash-table :test #'equal)
               :accessor atom-table))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-atom ()
  ((ns :initarg :ns
       :initform nil
       :reader ns
       :documentation "The type of namespace, and consequently storage type, that an mx-atom has.")
   (key :initarg :key
        :initform nil
        :accessor key
        :documentation "The unique string to identify an mx-atom in the universe.")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The primary string value, along with embedded mx-atoms, of an mx-atom.")
   (mods :initarg :mods
         :initform nil
         :accessor mods
         :documentation "The slot for selectors and transforms."))
  (:documentation "The base class for mx-atoms."))

(defclass mx-atom-data (mx-atom)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "A unique integer to identify an mx-atom in the mx-universe.")
   (metadata :initarg :metadata
             :initform nil
             :accessor metadata
             :documentation "The slot for additional information about an mx-atom.")
   (hash :initarg :hash
         :initform nil
         :accessor hash
         :documentation "The SHA256 checksum of an mx-atom.")
   (comment :initarg :comment
            :initform nil
            :accessor comment
            :documentation "The optional free-form string about an mx-atom.")
   (canonizedp :initarg :canonizedp
               :initform nil
               :accessor canonizedp
               :documentation "The flag to indicate whether an mx-atom has canon values."))
  (:documentation "The class for containing primary data about an atom."))

(defclass mx-atom-modsdata (mx-atom)
  ((metadata :initarg :metadata
             :initform nil
             :accessor metadata
             :documentation "The slot for additional information about mods.")
   (comment :initarg :comment
            :initform nil
            :accessor comment
            :documentation "The optional free-form string about mods."))
  (:documentation "The class for containing information about an atom’s mods."))

(defclass mx-atom-metadata (mx-atom)
  ((ns :initarg :ns
       :initform ":"
       :accessor ns
       :documentation "The namespace of a metadata which defaults to :.")
   (flag :initarg :flag
         :initform nil
         :accessor flag
         :documentation "Placeholder flag"))
  (:documentation "The class for containing atom metadata in the colon namespace."))

(defmacro update-counter (mx-universe accessor)
  "Update the counter in MX-UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,mx-universe))
          (,accessor ,mx-universe)))

(defmacro define-updaters (&rest namespaces)
  "Define functions for updating the namespace counters in the mx-universe."
  `(progn
     ,@(loop :for namespace :in namespaces
             :for fname = (marie:hyphenate-intern nil "update" namespace "counter")
             :for cname = (marie:hyphenate-intern nil namespace "counter")
             :collect `(defun ,fname (mx-universe)
                         (update-counter mx-universe ,cname)))))
(define-updaters machine world stream view canon atom)

(defun entity-string (id)
  "Return the corresponding universe name from ID, where ID is either a single
character or a string to designate an entity."
  (cdr (assoc id streams/specials:*namespaces-names* :test #'equal)))

(defun table-name (ns &optional package)
  "Return the corresponding table of NS from the universe."
  (let ((name (entity-string ns)))
    (marie:hyphenate-intern package name "table")))

(defun make-mx-atom-data (seq &optional value mods metadata hash comment)
  "Return a new MX-ATOM-DATA instance."
  (destructuring-bind (ns key)
      seq
    (make-instance 'mx-atom-data
                   :ns ns :key key :value value
                   :mods mods :metadata metadata
                   :hash hash :comment comment
                   :mx-universe streams/specials:*mx-universe*)))

(defun make-mx-atom-modsdata (seq &optional value mods metadata hash comment)
  "Return a new MX-ATOM-MODSDATA instance."
  (destructuring-bind (ns key)
      seq
    (make-instance 'mx-atom-modsdata
                   :ns ns :key key :value value
                   :mods mods :metadata metadata
                   :hash nil :comment comment)))

(defun make-mx-atom-metadata (seq &optional value mods)
  "Return a new MX-ATOM-METADATA instance."
  (destructuring-bind (ns key)
      seq
    (make-instance 'mx-atom-metadata
                   :ns ns :key key :value value :mods mods)))

(defmethod initialize-instance :after ((mx-atom-data mx-atom-data) &key mx-universe)
  "Initialize mx-atom MX-ATOM in mx-universe MX-UNIVERSE."
  (let ((counter (update-atom-counter mx-universe)))
    (with-slots (id ns key metadata table)
        mx-atom-data
      (setf id counter)
      (with-slots (atom-table)
          mx-universe
        (setf (gethash key atom-table) mx-atom-data)))))

(defmethod print-object ((mx-atom-data mx-atom-data) stream)
  (print-unreadable-object (mx-atom-data stream :type t)
    (with-slots (id ns key)
        mx-atom-data
      (format stream "~A ~A ~A" id ns key))))

(defmethod print-object ((mx-atom-metadata mx-atom-metadata) stream)
  (print-unreadable-object (mx-atom-metadata stream :type t)
    (with-slots (ns key)
        mx-atom-metadata
      (format stream "~A ~A" ns key))))

(defmethod print-object ((mx-atom-modsdata mx-atom-modsdata) stream)
  (print-unreadable-object (mx-atom-modsdata stream :type t)
    (with-slots (ns key)
        mx-atom-modsdata
      (format stream "~A ~A" ns key))))

(defmethod print-object ((h hash-table) stream)
  (print-unreadable-object (h stream :type t)
    (let ((test (hash-table-test h))
          (count (hash-table-count h)))
      (format stream "~A ~A" test count))))

(defun make-mx-universe ()
  "Return an instance of the mx-universe class."
  (make-instance 'mx-universe))
