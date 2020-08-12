;;;; common.lisp

(uiop:define-package #:streams/common
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:marie))

(in-package #:streams/common)

(defun ns-member-p (elem ns-list)
  "Return true if elem is a MEMBER of NS-LIST by CAR."
  (when* (member elem ns-list :key #'car :test #'equalp)))

(defun* @-namespace-p (ns)
  "Return true if NS is a base namespace indicator."
  (ns-member-p ns +@-namespace-list+))

(defun* atom-namespace-p (ns)
  "Return true if NS is a atom namespace indicator."
  (ns-member-p ns +atom-namespace-list+))

(defun* base-namespace-p (ns)
  "Return true if NS is a base namespace indicator."
  (ns-member-p ns +base-namespace-list+))

(defun* sub-namespace-p (ns)
  "Return true if NS is a sub namespace indicator."
  (ns-member-p ns +sub-namespace-list+))

(defun* colon-namespace-p (ns)
  "Return true if NS is a colon namespace indicator."
  (ns-member-p ns +colon-namespace-list+))

(defun* metadata-namespace-p (ns)
  "Return true if NS is a metadata namespace indicator."
  (ns-member-p ns +metadata-namespace-list+))

(defun* namespacep (ns)
  "Return true if NS is a namespace indicator."
  (rmap-or ns #'base-namespace-p #'sub-namespace-p))

(defun* object-slots (object)
  "Return the slot names of OBJECT."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun* slots (class)
  "Return the slot names of CLASS."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (find-class class))))

(defun* dump-object (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (object-slots object)
        :do (let ((v (funcall slot object)))
              (format t "~S -> ~S~%" slot v)
              (when (hash-table-p v)
                (dump-table v)))))

(defun* (dump-universe dump) (&optional (universe *universe*))
  "Dump the contents of the universe."
  (let* ((slots (object-slots universe))
         (string-slots (mapcar #'string* slots))
         (table-readers (loop :for item :in string-slots
                              :when (search "TABLE" item)
                              :collect item)))
    (format t "UNIVERSE: ~A~%" universe)
    (loop :for table-reader :in table-readers
          :for table = (funcall (intern (string* table-reader)
                                        (find-package :streams/classes))
                                universe)
          :do (progn
                (format t "~%~A:~%" table-reader)
                (dump-table* table)))))

(defun* dump-table (table)
  "Print information about TABLE recursively."
  (dump-table* table))

(defun* dump-path (table path)
  "Print the information in TABLE specified by PATH."
  (cond ((singlep path)
         (multiple-value-bind (val existsp)
             (gethash (single path) table)
           (when existsp
             (cond ((hash-table-p val) (dump-table val))
                   (t (format t "~S~%" val))))))
        ((hash-table-p (gethash (car path) table))
         (dump-path (gethash (car path) table) (cdr path)))
        (t nil)))

(defun* clear-table (table)
  "Clear all the contents of TABLE."
  (clrhash table))

(defun* tables (&optional (universe *universe*))
  "Return the list of slots from UNIVERSE that are tables."
  (loop :for slot :in (object-slots universe)
        :for tab = (funcall slot universe)
        :when (hash-table-p tab)
        :collect tab))

(defun* (clear-universe clear) (&optional (universe *universe*))
  "Set the current universe to an empty state."
  (loop :for table :in (tables universe) :do (clear-table table)))

(defun* copy-universe (universe)
  "Return a copy of the universe UNIVERSE, but with a new log date. The tables are copied using an external function to allow selective table information copying."
  (with-slots (atom-counter atom-table sub-atom-counter sub-atom-table)
      universe
    (make-instance 'universe
                   :atom-counter atom-counter
                   :atom-table (copy-table atom-table)
                   :sub-atom-counter sub-atom-counter
                   :sub-atom-table (copy-table sub-atom-table))))

(defun* copy-table (table)
  "Create a new hash table from TABLE."
  (let ((ht (make-hash-table :test (hash-table-test table)
                             :rehash-size (hash-table-rehash-size table)
                             :rehash-threshold (hash-table-rehash-threshold table)
                             :size (hash-table-size table))))
    (loop :for key :being :the :hash-key :of table
          :using (hash-value value)
          :do (setf (gethash key ht) value)
          :finally (return ht))))

(defun* clear-path (table path)
  "Remove the specified entry in TABLE that matches PATH."
  (labels ((fn (tab location)
             (cond ((singlep location)
                    (remhash (single location) tab)
                    table)
                   ((hash-table-p (gethash (car location) tab))
                    (fn (gethash (car location) tab) (cdr location)))
                   (t nil))))
    (fn table path)))

(defun clear-other (table key)
  "Remove entries in TABLE that do not match KEY."
  (let ((keys (loop :for k :being :the :hash-key :of table :collect k)))
    (loop :for item :in (remove key keys :test #'equalp)
          :do (remhash item table)
          :finally (return table))))

(defun* filter-path (table path)
  "Remove all other table entries in SOURCE that do not match PATH."
  (labels ((fn (tab location)
             (cond ((and (singlep location)
                         (hash-table-p (gethash (car location) tab)))
                    (clear-other tab (car location))
                    table)
                   ((hash-table-p (gethash (car location) tab))
                    (clear-other tab (car location))
                    (fn (gethash (car location) tab) (cdr location)))
                   (t nil))))
    (fn table path)))

(defun* termsp (form &optional (predicate #'namespacep))
  "Return true if FORM is a valid MSL form."
  (flet ((fn (form)
           (destructuring-bind (&optional head &rest _)
               form
             (declare (ignore _))
             (when*
               (consp head)
               (destructuring-bind (value &rest _)
                   head
                 (declare (ignore _))
                 (and (consp value)
                      (funcall predicate (car value))))))))
    (cond ((rmap-or form #'stringp #'numberp) nil)
          (t (fn form)))))

(defmacro* define-parser (name args &body body)
  "Define a function for defining parsers. NAME is the name of the parser
function; ARGS are the arguments passed to a parser—usually NIL; and BODY is the
body contents of the parser function."
  (let ((fname (string name)))
    (if (not (some #'(lambda (char) (char= (elt fname 0) char))
                   '(#\= #\? #\%)))
        (error "The parser name must start with =, ?, or %.")
        `(progn
           (export ',name)
           (defun ,name ,args ,@body)
           (setf (fdefinition ',(intern (subseq (string name) 1))) (,name))))))

(defun* current-date-iso-8601 ()
  "Return the current date and time in ISO 8601 format."
  (local-time:format-timestring nil (local-time:now)))

(defun* current-date-mimix ()
  "Return the current date and time in Mimix format."
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) #\- (:month 2) #\- (:day 2)
              #\@
              (:hour 2) #\- (:min 2) #\- (:sec 2)
              #\.
              (:usec 6) :gmt-offset-hhmm)))

(defun* debug-print (text &optional (stream *standard-output*))
  "Display TEXT to STREAM prefixing it with the the current date and time."
  (when *debug-print*
    (format stream "[~A] ~A~%" (current-date-mimix) text)
    (force-output stream)))

(defun* format-parens (&rest args)
  "Return a string formatted for MSL."
  (fmt "(~{~A~^ ~})" args))

(defun* clear-dump ()
  "Call CLEAR and DUMP."
  (clear-universe)
  (dump-universe))

(defmacro* with-fresh-universe (&body body)
  "Evaluate BODY in a separate universe."
  `(let ((streams/specials:*universe* (streams/classes:make-universe)))
     ,@body))

(defun* uncomment (expr)
  "Return a new string from EXPR without the comment."
  (cl-ppcre:regex-replace-all " ?//.*[^)]" expr ""))

(defun* path-exists-p (head)
  "Return true if HEAD is a valid path.."
  (gethash* head (atom-table *universe*)))

(defun* ensure-regex-path (path)
  "Return a path with regex from PATH."
  (if (string= (last* path) "/")
      path
      (append path '("/"))))

(defun* prefix-terms (prefix terms)
  "Add PREFIX to the paths in TERMS."
  (when (termsp terms)
    (loop :for term :in terms
          :collect (destructuring-bind (path &optional &rest value)
                       term
                     (cons (append prefix path) value)))))

(defun* left-trim (string)
  "Return a new string from STRING without the leading whitespace."
  (string-left-trim *whitespace* string))

(defun* right-trim (string)
  "Return a new string from STRING without the trailing whitespace."
  (string-right-trim *whitespace* string))

(defun* trim (string)
  "Return a new string from STRING without the leading and trailing whitespaces."
  (string-trim *whitespace* string))
