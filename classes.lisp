;;;; classes.lisp

(uiop:define-package #:streams/classes
  (:use #:cl)
  (:export #:mx-universe
           #:machine-table
           #:world-table
           #:stream-table
           #:view-table
           #:canon-table
           #:atom-table

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

           #:make-mx-atom-data
           #:make-mx-atom-metadata

           #:transforms
           #:selectors))

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
       :documentation "The type of namespace and consequently storage that an mx-atom has")
   (key :initarg :key
        :initform nil
        :accessor key
        :documentation "A unique string to identify an mx-atom in the universe.")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The primary text value, along with embedded mx-atoms, of an mx-atom.")
   (transforms :initarg :transforms
               :initform nil
               :accessor transforms
               :documentation "The slot for / and [] transforms")
   (selectors :initarg :selectors
              :initform nil
              :accessor selectors
              :documentation "The slot for F and D selectors"))
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
  (:documentation "The data class for containing information about an atom."))

(defclass mx-atom-metadata (mx-atom)
  ((flag :initarg :flag
         :initform nil
         :accessor flag
         :documentation "Placeholder flag"))
  (:documentation "The class for containing atom metadata stored in the colon space."))

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

(defun make-mx-atom-data (ns key &optional value transforms selectors metadata hash comment)
  "Return a new mx-atom-data instance."
  (make-instance 'mx-atom-data
                 :ns ns :key key :value value
                 :transforms transforms :selectors selectors
                 :metadata metadata
                 :hash hash :comment comment
                 :mx-universe streams/specials:*mx-universe*))

(defun make-mx-atom-metadata (key &optional value transforms selectors)
  "Return a new mx-atom-data instance."
  (make-instance 'mx-atom-metadata
                 :ns ":" :key key :value value
                 :transforms transforms :selectors selectors))

(defun initialize-mx-atom (mx-atom)
  "Initialize the metadata hash tables inside mx-atom to empty values."
  (let ((transforms streams/specials:*transform-indicators*)
        (selectors streams/specials:*selector-indicators*)
        (metadata (streams/classes:metadata mx-atom)))

    (setf (streams/classes:transforms mx-atom)
          (pairlis transforms '(nil nil)))

    (loop :for subtable :in selectors
          :do (setf (gethash subtable metadata)
                    (make-hash-table :test #'equal)))))

(defmethod initialize-instance :after ((mx-atom mx-atom) &key mx-universe)
  "Initialize mx-atom MX-ATOM in mx-universe MX-UNIVERSE."
  (let ((counter (update-atom-counter mx-universe)))
    (with-slots (id ns key metadata table)
        mx-atom
      (setf id counter)
      ;; (initialize-mx-atom mx-atom)
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

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun dump-mx-universe ()
  "Dump the contents of the mx-universe."
  (let* ((slots (slots streams/specials:*mx-universe*))
         (string-slots (mapcar #'marie:string-convert slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (loop :for table :in table-readers
          :do (progn
                (format t "> ~A~%" table)
                (marie:dump-table
                 (funcall (intern (marie:string-convert table)
                                  (find-package :streams/classes))
                          streams/specials:*mx-universe*))))))
