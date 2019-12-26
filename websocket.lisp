;;;; websocket.lisp

(in-package #:streams/core)


;;;-----------------------------------------------------------------------------
;;; General
;;;-----------------------------------------------------------------------------

(defvar *connections* (make-hash-table)
  "The table of connections, where the keys are server instances while the values are the user IDs.")

(defvar *user-base-id* 1000
  "The base integer user ID for connections.")

(defvar *servers* nil
  "A list of running websocket server instances.")

(defvar *system-version*
  #.(asdf:system-version (asdf:find-system :streams))
  "The introspected version of this system.")

(defun get-new-user-id ()
  "Return a new fresh user ID."
  (incf *user-base-id*))

(defun format-msl (type text)
  "Return a string formatted for MSL."
  (format nil "(@~A ~A)" type text))

(defmacro format-output (&rest args)
  "Apply CL:FORMAT to ARGS then force the output."
  (apply #'format args)
  (force-output *standard-output*))

(defun handle-open-connection (connection)
  "Add a new entry to the connections table, with the connection itself as the key."
  (setf (gethash connection *connections*)
        (format nil "user-~A" (get-new-user-id)))
  (format-output t "Connection: ~A" connection)
  (websocket-driver:send connection (format-msl "VER" *system-version*)))

(defun echo-message (connection message)
  "Echo back MESSAGE to CONNECTION."
  (websocket-driver:send connection message))

(defun handle-close-connection (connection)
  (let ((message (format nil " ... ~A has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections*
          :do (websocket-driver:send con message))))

(defun start-server (server port)
  "Start SERVER using Clack."
  (clack:clackup server :port port))

(defun stop-server (server)
  "Stop SERVER."
  (clack:stop server))


;;;-----------------------------------------------------------------------------
;;; MSL interface
;;;-----------------------------------------------------------------------------

(defvar *msl-start-port* 60000
  "The starting port for (msl) communication.")

(defvar *msl-server* nil
  "The msl server instance.")

(defun msl-server (env)
  "Handle requests to the msl server."
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-open-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (message) (echo-message ws message)))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

(defun start-msl-server ()
  "Start the msl websocket server."
  (let ((server (start-server #'msl-server *msl-start-port*)))
    (setf *msl-server* server)
    server))

(defun stop-msl-server ()
  "Stop the msl websocket server."
  (stop-server *msl-server*)
  (setf *msl-server* nil))


;;;-----------------------------------------------------------------------------
;;; Admin interface
;;;-----------------------------------------------------------------------------

(defvar *admin-start-port* 60500
  "The starting port for admin access.")

(defvar *admin-server* nil
  "The admin server instance.")

(defun admin-server (env)
  "Handle requests to the admin server."
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-open-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (message) (echo-message ws message)))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

(defun start-admin-server ()
  "Start the admin websocket server."
  (let ((server (start-server #'admin-server *admin-start-port*)))
    (setf *admin-server* server)
    server))

(defun stop-admin-server ()
  "Stop the admin websocket server."
  (stop-server *admin-server*)
  (setf *admin-server* nil))


;;;-----------------------------------------------------------------------------
;;; Top-level
;;;-----------------------------------------------------------------------------

(defun start-websocket-server (server &rest args)
  "Start the designated websocket server."
  (format t "Starting websocket server...~%")
  (let ((server (apply server args)))
    (push server *servers*)
    server))

(defun stop-websocket-servers ()
  "Stop all the websocket servers."
  (hide-debugger-output)
  (format *error-output* "Aborting.~&")
  (loop :for server :in *servers* :do (clack:stop server))
  (setf *servers* nil)
  (uiop:quit))

(defun main ()
  "The main entrypoint of the module."
  (start-websocket-server #'start-msl-server)
  (start-websocket-server #'start-admin-server)

  (handler-case (bt:join-thread (find-if (lambda (thread)
                                           (search "hunchentoot" (bt:thread-name thread)))
                                         (bt:all-threads)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     #+lispworks mp:process-interrupt
     () (stop-websocket-servers))
    (error (c)
      (format t "Oops, an unknown error occured:~&~A~&" c))))
