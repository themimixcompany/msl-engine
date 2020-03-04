;;;; channels.lisp

(uiop:define-package #:streams/channels
    (:use #:cl)
  (:nicknames #:s/channels)
  (:export
   ;; mx-universe
   #:mx-universe
   #:machine-table
   #:world-table
   #:stream-table
   #:view-table
   #:canon-table
   #:atom-table

   ;; mx-atom
   #:mx-atom
   #:id
   #:ns
   #:key
   #:value
   #:metadata
   #:hash
   #:comment

   #:make-mx-universe
   #:dump-mx-universe

   #:entity-string
   #:table-name

   #:make-mx-atom))

(in-package #:streams/channels)

(defclass mx-universe ()
  ((machine-counter :initarg :machine-counter
                    :initform streams/ethers:*initial-machine-counter*
                    :accessor machine-counter
                    :documentation "The top-level mx-machine counter.")
   (machine-table :initarg :machine-table
                  :initform (make-hash-table :test #'equal)
                  :accessor machine-table
                  :documentation "The top-level colletion of mx-machines, where the key is the name of mx-machine and the value is the instance of that mx-machine.")
   (world-counter :initarg :world-counter
                  :initform streams/ethers:*initial-world-counter*
                  :accessor world-counter
                  :documentation "The top-level mx-world counter.")
   (world-table :initarg :world-table
                :initform (make-hash-table :test #'equal)
                :accessor world-table
                :documentation "The top-level colletion of mx-worlds, where the key is the name of mx-world and the value is the instance of that mx-world.")
   (stream-counter :initarg :stream-counter
                   :initform streams/ethers:*initial-stream-counter*
                   :accessor stream-counter
                   :documentation "The top-level mx-stream counter.")
   (stream-table :initarg :stream-table
                 :initform (make-hash-table :test #'equal)
                 :accessor stream-table
                 :documentation "The top-level colletion of mx-streams, where the key is the name of mx-stream and the value is the instance of that mx-stream.")
   (view-counter :initarg :view-counter
                 :initform streams/ethers:*initial-view-counter*
                 :accessor view-counter
                 :documentation "The top-level mx-view counter.")
   (view-table :initarg :view-table
               :initform (make-hash-table :test #'equal)
               :accessor view-table
               :documentation "The top-level colletion of mx-views, where the key is the name of mx-view and the value is the instance of that mx-view.")
   (canon-counter :initarg :canon-counter
                  :initform streams/ethers:*initial-canon-counter*
                  :accessor canon-counter
                  :documentation "The top-level mx-canon counter.")
   (canon-table :initarg :canon-table
                :initform (make-hash-table :test #'equal)
                :accessor canon-table
                :documentation "The top-level colletion of mx-canons, where the key is the name of mx-canon and the value is the instance of that mx-canon.")
   (atom-counter :initarg :atom-counter
                 :initform streams/ethers:*initial-atom-counter*
                 :accessor atom-counter
                 :documentation "The top-level mx-atom counter.")
   (atom-table :initarg :atom-table
               :initform (make-hash-table :test #'equal)
               :accessor atom-table
               :documentation "The top-level colletion of mx-atoms, where the key is the name of the mx-atom and the value is the instance of that mx-atom."))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-atom ()
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric id of a mx-atom.")
   (ns :initarg :ns
       :initform nil
       :reader ns
       :documentation "The namespace key of an mx-atom, whether it is M, S, C, V, or @. This also determines what table to use in the universe.")
   (key :initarg :key
        :initform nil
        :accessor key
        :documentation "The name of an mx-atom.")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The primary value of an mx-atom.")
   (metadata :initarg :metadata
             :initform (make-hash-table :test #'equal)
             :accessor metadata
             :documentation "The secondary values of an mx-atom as a hash table, where the key is the type of metadata and the value is another table.")
   (hash :initarg :hash
         :initform nil
         :accessor hash
         :documentation "The SHA-256 hash of an atom.")
   (comment :initarg :comment
            :initform nil
            :accessor comment
            :documentation "Free form text about an atom.")
   (table :initarg :table
          :initform nil
          :accessor table
          :documentation "The corresponding table assignment of this atom in the universe.")
   (canonizedp :initarg :canonizedp
               :initform nil
               :accessor canonizedp
               :documentation "A flag to indicate whether an mx-atom’s contents has been canonized."))
  (:documentation "The structure to designate atoms."))

(defmacro update-counter (mx-universe accessor)
  "Update the counter in MX-UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,mx-universe))
          (,accessor ,mx-universe)))

(defmacro define-updaters (&rest namespaces)
  "Define functions for updating the namespace counters in the mx-universe."
  `(progn
     ,@(loop :for namespace :in namespaces
             :for fname = (mof:hyphenate-intern nil "update" namespace "counter")
             :for cname = (mof:hyphenate-intern nil namespace "counter")
             :collect `(defun ,fname (mx-universe)
                         (update-counter mx-universe ,cname)))))
(define-updaters machine world stream view canon atom)

(defun entity-string (id)
  "Return the corresponding universe name from ID, where ID is either a single
character or a string to designate an entity."
  (cdr (assoc id streams/ethers:*namespaces-names* :test #'equal)))

(defun table-name (ns &optional package)
  "Return the corresponding table of NS from the universe."
  (let ((name (entity-string ns)))
    (mof:hyphenate-intern package name "table")))

(defun make-mx-atom (ns key &optional value (metadata (make-hash-table :test #'equal)) hash comment)
  "Return a new mx-atom instance from arguments."
  (make-instance 'mx-atom :ns ns :key key
                          :value value :metadata metadata
                          :hash hash :comment comment
                          :mx-universe streams/ethers:*mx-universe*))

(defun initialize-metatable (mx-atom)
  "Initialize the metadata hash tables inside mx-atom to empty values."
  (loop :for subtable :in streams/ethers:*metadata-prefixes*
        :with metadata = (streams/channels:metadata mx-atom)
        :do (setf (gethash subtable metadata)
                  (make-hash-table :test #'equal))))

(defmethod initialize-instance :after ((mx-atom mx-atom) &key mx-universe)
  "Initialize mx-atom MX-ATOM in mx-universe MX-UNIVERSE."
  (let ((counter (update-atom-counter mx-universe)))
    (with-slots (id ns key metadata table)
        mx-atom
      (setf id counter)
      (setf table (table-name ns))
      (initialize-metatable mx-atom)
      (with-slots (atom-table)
          mx-universe
        (setf (gethash key atom-table) mx-atom)))))

(defmethod print-object ((mx-atom mx-atom) stream)
  (print-unreadable-object (mx-atom stream :type t)
    (with-slots (ns id key)
        mx-atom
      (format stream "~A ~A ~A" id ns key))))

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
                 (funcall (intern (streams/common:string-convert table)
                                  (find-package :streams/channels))
                          streams/ethers:*mx-universe*))))))
