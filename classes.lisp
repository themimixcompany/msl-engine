;;;; classes.lisp

(uiop:define-package #:streams/classes
  (:use #:cl)
  (:export #:mx-universe

           #:atom-table
           #:atom-counter
           #:sub-atom-table
           #:sub-atom-counter
           #:metadata-table
           #:metadata-counter

           #:mx-base
           #:mx-atom
           #:mx-sub-atom
           #:mx-metadata

           #:make-mx-atom
           #:build-mx-atom
           #:make-mx-sub-atom
           #:build-mx-sub-atom
           #:make-mx-metadata
           #:build-mx-metadata

           ;; Think about the irrelevance of some of these symbols
           #:id
           #:ns
           #:key
           #:value
           #:canonize

           #:make-mx-universe
           #:canonizedp))

(in-package #:streams/classes)

(defclass mx-universe ()
  ((atom-counter :initarg :atom-counter
                 :initform streams/specials:*atom-counter*
                 :accessor atom-counter
                 :documentation "The global integer counter for base atoms.")
   (atom-table :initarg :atom-table
               :initform (make-hash-table :test #'equal)
               :accessor atom-table
               :documentation "The table where all base atoms live.")
   (sub-atom-counter :initarg :sub-atom-counter
                     :initform streams/specials:*sub-atom-counter*
                     :accessor sub-atom-counter
                     :documentation "The global integer counter for sub atoms.")
   (sub-atom-table :initarg :sub-atom-table
                   :initform (make-hash-table :test #'equal)
                   :accessor sub-atom-table
                   :documentation "The table where sub atoms live.")
   (metadata-counter :initarg :metadata-counter
                     :initform streams/specials:*metadata-counter*
                     :accessor metadata-counter
                     :documentation "The global integer counter for metadata.")
   (metadata-table :initarg :metadata-table
                   :initform (make-hash-table :test #'equal)
                   :accessor metadata-table
                   :documentation "The table where metadata things live."))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-base ()
  ((ns :initarg :ns
       :initform nil
       :reader ns
       :documentation "The type of namespace, and consequently storage type, that an mx-base has.")
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

(defclass mx-sub-atom (mx-base)
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique integer to identify the mx-sub-atom in the universe."))
  (:documentation "The class for mx-sub-atoms, and instances are allocated on the universe."))

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
(define-updaters atom sub-atom)

(defmacro define-maker (class &key allocate)
  "Define functions for MX classes. CLASS is the name of MX class to be
instantiated. ALLOCATE is a boolean whether to allocate the instance on the universe."
  (flet ((make-name (&rest args)
           (apply #'marie:hyphenate-intern nil args)))
    (let* ((mx-name (make-name "mx" class))
           (maker-name (make-name "make" mx-name))
           (builder-name (make-name "build" mx-name))
           (updater-name (make-name "update" class "counter"))
           (table-name (make-name class "table")))
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
(define-makers ((atom t) (sub-atom t) (metadata)))

(defun make-mx-universe ()
  "Return an instance of the mx-universe class."
  (make-instance 'mx-universe))

(defun canonize (mx-atom)
  "Set the canonized flag of MX-ATOM to true, irrespective of its existing value."
  (setf (canonizedp mx-atom) t)
  mx-atom)
