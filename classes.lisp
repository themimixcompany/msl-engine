;;;; classes.lisp

(uiop:define-package #:streams/classes
  (:use #:cl)
  (:export #:mx-universe

           #:atom-table
           #:atom-counter
           #:datatype-table
           #:datatype-counter
           #:format-table
           #:format-counter

           #:mx-base

           #:mx-atom
           #:make-mx-atom
           #:build-mx-atom

           #:mx-datatype
           #:make-mx-datatype
           #:build-mx-datatype

           #:mx-format
           #:make-mx-format
           #:build-mx-format

           #:mx-metadata
           #:make-mx-metadata
           #:build-mx-metadata

           #:id
           #:ns
           #:value

           #:make-mx-universe
           #:make-sub-tables
           #:canonize))

(in-package #:streams/classes)

(defclass mx-universe ()
  ((atom-counter :initarg :atom-counter
                 :initform streams/specials:*atom-counter*
                 :accessor atom-counter)
   (atom-table :initarg :atom-table
               :initform (make-hash-table :test #'equal)
               :accessor atom-table)
   (datatype-counter :initarg :datatype-counter
                     :initform streams/specials:*datatype-counter*
                     :accessor datatype-counter)
   (datatype-table :initarg :datatype-table
                   :initform (make-hash-table :test #'equal)
                   :accessor datatype-table)
   (format-counter :initarg :format-counter
                   :initform streams/specials:*format-counter*
                   :accessor format-counter)
   (format-table :initarg :format-table
                 :initform (make-hash-table :test #'equal)
                 :accessor format-table)
   (metadata-counter :initarg :metadata-counter
                     :initform streams/specials:*metadata-counter*
                     :accessor metadata-counter)
   (metadata-table :initarg :metadata-table
                   :initform (make-hash-table :test #'equal)
                   :accessor metadata-table))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-base ()
  ((ns :initarg :ns
       :initform nil
       :reader ns
       :documentation "The type of namespace, and consequently storage type, that an mx-atom has.")
   (key :initarg :key
        :initform nil
        :accessor key
        :documentation "The unique string to identify an mx-atom in the universe.")
   (value :initarg :value
          :initform (make-hash-table :test #'equal)
          :accessor value
          :documentation "The hash table to store everything local to an atom."))
  (:documentation "The base class for everything."))

(defclass mx-atom (mx-base)
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique integer to identify the mx-atom in the universe.")
   (canonizedp :initarg :canonizedp
               :initform nil
               :accessor canonizedp
               :documentation "The flag to indicate whether an mx-atom has canon values."))
  (:documentation "The class for atom data."))

(defclass mx-datatype (mx-base)
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique integer to identify the mx-datatype in the universe."))
  (:documentation "The class for datatypes, and instances are allocated on the universe."))

(defclass mx-format (mx-base)
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique integer to identify the mx-format in the universe."))
  (:documentation "The class for formats, and instances are allocated on the universe."))

(defclass mx-metadata (mx-base)
  ()
  (:documentation "The class for metadata, and instances are not allocated on the universe."))

(defmacro update-counter (mx-universe accessor)
  "Update the counter in MX-UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,mx-universe))
          (,accessor ,mx-universe)))

(defmacro define-updaters (&rest namespaces)
  "Define functions for updating the namespace counters in the mx-universe."
  (flet ((make-name (&rest args)
           (apply #'marie:hyphenate-intern nil args)))
    `(progn
       ,@(loop :for namespace :in namespaces
               :for fname = (make-name "update" namespace "counter")
               :for cname = (make-name namespace "counter")
               :collect `(defun ,fname (mx-universe)
                           (update-counter mx-universe ,cname))))))
(define-updaters atom datatype format)

(defmacro define-maker (class &key allocate)
  "Define functions for MX classes. CLASS is the name of MX class to be
instantiated. ALLOCATE is a boolean whether to allocate the instance on the universe."
  (flet ((make-name (&rest args)
           (apply #'marie:hyphenate-intern nil args))
         (make-keys-name (class)
           (marie:cat-intern nil "*" (marie:hyphenate class "keys") "*")))
    (let* ((mx-name (make-name "mx" class))
           (maker-name (make-name "make" mx-name))
           (builder-name (make-name "build" mx-name))
           (updater-name (make-name "update" class "counter"))
           (table-name (make-name class "table"))
           (keys (make-keys-name class)))
      (declare (ignorable keys))
      `(progn
         (defun ,maker-name (seq &optional value force)
           (destructuring-bind (ns key)
               seq
             (let ((obj (gethash key (,table-name streams/specials:*mx-universe*))))
               (flet ((fn ()
                        (make-instance ',mx-name :ns ns :key key :value value
                                       ,@(when allocate
                                           `(:mx-universe streams/specials:*mx-universe*)))))
                 (cond (force (fn))
                       (t (or obj (fn))))))))
         (defun ,builder-name (args)
           (when args
             (apply #',maker-name args)))
         ,(when allocate
            `(defmethod initialize-instance :after ((,mx-name ,mx-name) &key mx-universe)
               (let ((counter (,updater-name mx-universe)))
                 (with-slots (id ns key value)
                     ,mx-name
                   (setf id counter)
                   (with-slots (,table-name)
                       mx-universe
                     (setf (gethash key ,table-name) ,mx-name))))))
         (defmethod print-object ((,mx-name ,mx-name) stream)
           (print-unreadable-object (,mx-name stream :type t)
             (with-slots (id ns key)
                 ,mx-name
               (format stream "~A ~A ~A" id ns key))))))))

(defmacro define-makers (specs)
  "Define MX structure makers and helpers with DEFINE-MAKER."
  `(progn ,@(loop :for spec :in specs :collect
                     (destructuring-bind (name &optional allocate)
                         spec
                       `(define-maker ,name :allocate ,allocate)))))
(define-makers ((atom t) (datatype t) (format t) (metadata)))

(defun make-mx-universe ()
  "Return an instance of the mx-universe class."
  (make-instance 'mx-universe))

(defun make-sub-tables (obj)
  "Define sub tables under OBJ. This is mainly used by metadata."
  (let ((table (streams/classes:value mx-atom-data))
        (sub-tables '("=" "/" "f" "d")))
    (loop :for sub :in sub-tables
          :do (setf (gethash sub table) (make-hash-table :test #'equal)))))

(defun canonize (mx-atom)
  "Set the canonized flag of MX-ATOM to true, irrespective of its existing value."
  (setf (canonizedp mx-atom) t)
  mx-atom)
