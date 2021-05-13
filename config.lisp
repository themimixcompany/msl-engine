;;;; config.lisp

(uiop:define-package #:msl-engine/config
  (:use #:cl
        #:msl-engine/specials
        #:marie))

(in-package #:msl-engine/config)

(def config-directory ()
  "Return the path to the default configuration and storage directory."
  (~ (cat #\. +self+ #\/)))

(defv *default-config*
  '((listen-address "127.0.0.1"))
  "The default configuration.")

(defv *config-file*
    (uiop:merge-pathnames* "config.lisp" (config-directory))
  "The location of the config file on the disk")

(defun config-file-exists-p ()
  "Return true if the config file exists."
  (when (uiop:file-exists-p *config-file*)
    t))

(defun read-config-file ()
  "Read the configuration file."
  (uiop:read-file-forms *config-file*))

(defun read-config ()
  "Return the most proximate configuration."
  (if (config-file-exists-p)
      (read-config-file)
      *default-config*))

(def config-value (index)
  "Return the value associated with an index."
  (let ((config (read-config)))
    (car (assoc-value index config))))
