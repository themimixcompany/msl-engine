;;;; specials.lisp

(uiop:define-package #:streams/specials
  (:use #:cl
        #:marie))

(in-package #:streams/specials)

(defv +self+
  "streams"
  "The base name of the system.")

(defv *universe* nil
  "The top-level structure for everything.")

(defv *atom-counter* 100
  "The initial mx-atom counter value.")

(defv *sub-atom-counter* 1000
  "The initial mx-sub-atom counter value.")

(defv *metadata-counter* 10000
  "The initial metadata counter value.")

(eval-always
  (defc +@-ns-list+
      '(("@" . "@"))
    "The list of @ nss.")

  (defc +atom-ns-list+
      '(("msl" . "prelude")
        ("c"   . "canon")
        ("m"   . "machine")
        ("w"   . "world")
        ("s"   . "stream")
        ("v"   . "view"))
    "The list of atom nss.")

  (defc +base-ns-list+
      (append +@-ns-list+
              +atom-ns-list+)
    "The list of base nss.")

  (defc +sub-ns-list+
      '(("d" . "datatype")
        ("f" . "format"))
    "The list of sub nss.")

  (defc +colon-ns-list+
      '((":" . "colon"))
    "The list of colon nss.")

  (defc +metadata-ns-list+
      '((":" . "metadata"))
    "The list of metadata nss."))

(defc +key-indicators+
    '("=" "/" "[]")
  "The list of strings used for setting end values.")

(defv *log-directory*
    (~ (cat #\. +self+ #\/))
  "The path to the default configuration and storage directory.")

(defc +log-file-suffix+
  "msl"
  "The default file suffix for log files.")

(defc +iso-8601-re+
  "\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d(\\.\\d+)?(([+-]\\d\\d:\\d\\d)|Z)?"
  "The regular expression for ISO 8601 dates.")

(defc +mimix-date-re+
  "\\d{4}-\\d\\d-\\d\\d@\\d\\d-\\d\\d-\\d\\d(\\.\\d+)?(([+-]\\d\\d\\d\\d)|Z)?"
  "The regular expression for Mimix dates.")

(defc +day-names+
    '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "The enumeration of week day names.")

(defp *maximum-log-size*
  5242880
  "The maximum filesize of logging files in bytes.")

(def system-object (name)
  "Return the system object for the current system."
  (asdf:find-system (intern name (find-package :keyword))))

(def self-asdf ()
  "Return the ASDF file path for the current system."
  (uiop:merge-pathnames* (cat +self+ ".asd")
                         (asdf:system-source-directory (system-object +self+))))

(def read-self-asdf ()
  "Return the system ASDF file as s-expressions."
  (uiop:read-file-forms (self-asdf)))

(def system-version (name)
  "Return the version number extracted from the system resources."
  (let* ((system (system-object name))
         (asdf-base-name (cat name ".asd"))
         (source-directory (asdf:system-source-directory system))
         (forms (uiop:read-file-forms (uiop:merge-pathnames* asdf-base-name source-directory))))
    (getf (assoc 'defsystem forms :test #'equal) :version)))

(defv *machine*
  ;;(uiop:hostname)
  "my-machine"
  "The default name to use as the machine name.")

(defv *system-version*
  ;; (uiop:os-cond
  ;;  ((uiop:os-windows-p) (system-version +self+))
  ;;  (t (asdf:system-version (system-object +self+))))
  "2.4.40"
  "The introspected version of this system.")

(defv *slynk-port*
  4005
  "The default slynk communication port.")

(defv *debug-print*
  t
  "Whether to print debugging information using a dedicated outputter.")

(defv *restore-log*
  nil
  "Whether to restore data from the log files.")

(defv *whitespace*
    '(#\space #\tab #\newline #\rubout)
  "The list of whitespace characters.")
