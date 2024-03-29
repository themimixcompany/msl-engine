;;;; classes.lisp

(uiop:define-package #:msl-engine/classes
  (:use #:cl
        #:msl-engine/specials
        #:marie)
  (:export #:universe
           #:name

           #:atom-counter
           #:atom-table
           #:sub-atom-counter
           #:sub-atom-table
           #:log-date

           #:mx-base
           #:mx-atom
           #:mx-sub-atom

           #:id
           #:ns
           #:key
           #:value
           #:canonizedp

           #:table
           #:date))

(in-package #:msl-engine/classes)

(defclass universe ()
  ((name :initarg :name
         :initform (genstr "U")
         :accessor name
         :documentation "The name of the universe")
   (atom-counter :initarg :atom-counter
                 :initform *atom-counter*
                 :accessor atom-counter
                 :documentation "The global integer counter for base atoms.")
   (atom-table :initarg :atom-table
               :initform (make-hash-table :test #'equalp)
               :accessor atom-table
               :documentation "The table where all base atoms live.")
   (sub-atom-counter :initarg :sub-atom-counter
                     :initform *sub-atom-counter*
                     :accessor sub-atom-counter
                     :documentation "The global integer counter for sub atoms.")
   (sub-atom-table :initarg :sub-atom-table
                   :initform (make-hash-table :test #'equalp)
                   :accessor sub-atom-table
                   :documentation "The table where sub atoms live.")
   (log-date :initarg :log-date
             :initform (local-time:format-timestring
                        nil (local-time:now)
                        :format `((:year 4) #\- (:month 2) #\- (:day 2)
                                  #\@
                                  (:hour 2) #\- (:min 2) #\- (:sec 2)
                                  #\.
                                  (:usec 6) :gmt-offset-hhmm))
             :accessor log-date
             :documentation "The date and time value used for logging."))
  (:documentation "The top-level data structure for mx-atoms including information about the current mx-atom counter and the main table."))

(defclass mx-base ()
  ((ns :initarg :ns
       :initform nil
       :reader ns
       :documentation "The type of ns, and consequently storage type, that an mx-base has.")
   (key :initarg :key
        :initform nil
        :accessor key
        :documentation "The unique string to identify an mx-atom in the universe.")
   (value :initarg :value
          :initform (make-hash-table :test #'equalp)
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

(defmacro update-counter (universe accessor)
  "Update the counter in UNIVERSE with ACCESSOR."
  `(progn (incf (,accessor ,universe))
          (,accessor ,universe)))

(defmacro def-updaters (&rest nss)
  "Define functions for updating the ns counters in the universe."
  (flet ((make-name (&rest args)
           (apply #'hyphenate-intern nil args)))
    `(progn
       ,@(loop :for ns :in nss
               :for fname = (make-name "update" ns "counter")
               :for cname = (make-name ns "counter")
               :collect `(defun ,fname (universe)
                           (update-counter universe ,cname))))))
(def-updaters atom sub-atom)

(defmacro def-maker (class &key allocate)
  "Define functions for MX classes. CLASS is the name of MX class to be
instantiated. ALLOCATE is a boolean whether to allocate the instance on the universe."
  (flet ((make-name (&rest args)
           (apply #'hyphenate-intern nil args)))
    (let* ((mx-name (make-name "mx" class))
           (maker-name (make-name "make" mx-name))
           (builder-name (make-name "build" mx-name))
           (updater-name (make-name "update" class "counter"))
           (table-name (make-name class "table"))
           (clear-table-name (make-name "clear" table-name)))
      `(progn
         (defun ,maker-name (seq &optional value force)
           (destructuring-bind (ns key)
               seq
             (let ((obj (gethash key (,table-name *universe*))))
               (flet ((fn ()
                        (make-instance ',mx-name :ns ns :key key :value value
                                       ,@(when allocate
                                           `(:universe *universe*)))))
                 (cond (force (fn))
                       (t (or obj (fn))))))))
         (defun ,builder-name (args)
           (when args
             (apply #',maker-name args)))
         (defun ,clear-table-name ()
           (clrhash (,table-name *universe*)))
         ,(when allocate
            `(defmethod initialize-instance :after ((,mx-name ,mx-name) &key universe)
               (let ((counter (,updater-name universe)))
                 (with-slots (id ns key value)
                     ,mx-name
                   (setf id counter)
                   (with-slots (,table-name)
                       universe
                     (setf (gethash key ,table-name) ,mx-name))))))
         (defmethod print-object ((,mx-name ,mx-name) stream)
           (print-unreadable-object (,mx-name stream :type t)
             (with-slots (id ns key)
                 ,mx-name
               (format stream "~A ~A ~A" id ns key))))
         (export ',maker-name)
         (export ',builder-name)))))

(defmacro def-makers (specs)
  "Define MX structure makers and helpers with DEF-MAKER."
  `(progn ,@(loop :for spec :in specs :collect
                     (destructuring-bind (name &optional allocate)
                         spec
                       `(def-maker ,name :allocate ,allocate)))))
(def-makers ((atom t) (sub-atom t)))

(def make-universe (&optional name)
  "Return an instance of the universe class."
  (make-instance 'universe :name (or name (genstr "U"))))

(defmethod print-object ((u universe) stream)
  (print-unreadable-object (u stream :type t)
    (with-slots (name)
        u
      (format stream "~A" name))))

#+lispworks
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *lw-saved-handle-warn-on-redefinition* lw:*handle-warn-on-redefinition*)
  (setq lw:*handle-warn-on-redefinition* :quiet))

(defmethod print-object ((table hash-table) stream)
  (print-unreadable-object (table stream :type t)
    (let ((test (hash-table-test table))
          (count (hash-table-count table)))
      (format stream "~A ~A" test count))))

#+lispworks
(eval-when (:compile-toplevel :load-toplevel)
  (setq lw:*handle-warn-on-redefinition* *lw-saved-handle-warn-on-redefinition*))

(defclass register ()
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique integer to identify the register.")
   (table :initarg :table
          :initform (make-hash-table :test #'equalp)
          :reader table
          :documentation "The hash table for the data store.")
   (date :initarg :date
         :initform (local-time:format-timestring nil (local-time:now))
         :accessor date
         :documentation "The date and time associated with a register."))
  (:documentation "The register class."))
