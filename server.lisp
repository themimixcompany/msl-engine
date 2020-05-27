;;;; server.lisp

(uiop:define-package #:streams/server
  (:use #:cl
        #:websocket-driver
        #:streams/specials
        #:streams/common
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        #:streams/admin-dispatcher
        #:marie)
  (:export #:server))

(in-package #:streams/server)


;;--------------------------------------------------------------------------------------------------
;; general helpers
;;--------------------------------------------------------------------------------------------------

(defvar *user-connections* (make-hash-table)
  "The table of connections, where the keys are server instances while the values are the user IDs.")

(defvar *user-base-id* 1000
  "The base integer user ID for connections.")

(defun get-new-user-id ()
  "Return a new fresh user ID."
  (incf *user-base-id*)
  *user-base-id*)


;;--------------------------------------------------------------------------------------------------
;; generic handlers
;;--------------------------------------------------------------------------------------------------

(defun post (wire data)
  "Send DATA to WIRE if it contains valid information."
  (when (and wire data)
    (send wire data)))

(defun format-msl (type text)
  "Return a string formatted for MSL."
  (format nil "(@~A ~A)" type text))

(defun handle-open (connection)
  "Process incoming connection CONNECTION."
  (let ((uid (get-new-user-id)))
    (setf (gethash connection *user-connections*)
          (format nil "~A" uid))
    ;;(send connection (format-msl "VER" *system-version*))
    (post connection (admin-dispatch "(@VER)"))))

(defun echo-message (connection message)
  "Send back MESSAGE to CONNECTION."
  (send connection message))

(defun handle-close (connection)
  "Process connection CONNECTION when it closes."
  (let ((message (format nil " ... ~A has left."
                         (gethash connection *user-connections*))))
    (remhash connection *user-connections*)
    (loop :for con :being :the :hash-key :of *user-connections*
          :do (send con message))))


;;--------------------------------------------------------------------------------------------------
;; main handlers
;;--------------------------------------------------------------------------------------------------

(defvar *servers* nil
  "The list of active servers.")

(defun clack-start (server port)
  "Start the clack server SERVER under port PORT."
  (clack:clackup server :port port))

(defun clack-stop (server)
  "Stop the clack server SERVER."
  (clack:stop server))

(defmacro* define-runners (name form port open-handler message-handler close-handler)
  "Define functions for executing server operations."
  (declare (ignorable form))
  (flet ((make-name (&rest args)
           (apply #'hyphenate-intern nil args)))
    (let* ((server-name (make-name name "server"))
           (start-server-name (make-name "start" server-name))
           (stop-server-name (make-name "stop" server-name))
           (server-symbol-name (cat-intern nil "*" server-name "*"))
           (wire-symbol-name (cat-intern nil "*" (make-name name "wire") "*")))
      `(progn
         (defvar ,server-symbol-name nil)
         (defvar ,wire-symbol-name nil)
         (defun ,server-name (env)
           (let ((server (websocket-driver:make-server env)))
             (setf ,wire-symbol-name server)
             (websocket-driver:on :open server ,open-handler)
             (websocket-driver:on :message server ,message-handler)
             (websocket-driver:on :close server ,close-handler)
             (lambda (_)
               (declare (ignore _))
               (websocket-driver:start-connection server))))
         (defun ,start-server-name ()
           (let ((server (clack-start #',server-name ,port)))
             (setf ,server-symbol-name server)
             (pushnew server *servers* :test #'equal)
             server))
         (defun ,stop-server-name ()
           (clack-stop ,server-symbol-name)
           (remove ,server-symbol-name *servers* :test #'equal)
           (setf ,server-symbol-name nil))))))


;;--------------------------------------------------------------------------------------------------
;; entrypoints
;;--------------------------------------------------------------------------------------------------

(define-runners "Admin" 'admin 60500
  (lambda ()
    (handle-open server))
  (lambda (message)
    (post *admin-wire* (admin-dispatch message)))
  (lambda (&key _ __)
    (declare (ignore _ __))
    (handle-close server)))

(define-runners "MSL" 'msl 60000
  (lambda () (handle-open server))
  (lambda (message)
    (dispatch message)
    (post *msl-wire* (recall-expr message))
    (post *admin-wire* (recall-value message)))
  (lambda (&key _ __)
    (declare (ignore _ __))
    (handle-close server)))

(defun start-servers ()
  "Start all the servers."
  (format *error-output* "Loading servers...~%")
  (start-admin-server)
  (start-msl-server))

(defun stop-servers ()
  "Stop all the servers."
  (format *error-output* "Stopping servers...~%")
  (stop-msl-server)
  (stop-admin-server)
  (uiop:quit))

(defun* serve ()
  "The main entrypoint of the server."
  (format t "streams v~A~%" *system-version*)
  (start-servers)
  (handler-case (bt:join-thread (find-if (lambda (thread)
                                           (search "hunchentoot" (bt:thread-name thread)))
                                         (bt:all-threads)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     #+lispworks mp:process-interrupt
     () (stop-servers))
    (error (c)
      (format t "Oops, an unknown error occured:~&~A~&" c)))
  nil)
