;;;; server.lisp

(uiop:define-package #:streams/server
  (:use #:cl
        #:streams/specials
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        #:marie))

(in-package #:streams/server)

(defvar *connections* (make-hash-table)
  "The table of connections, where the keys are server instances while the values are the user IDs.")

(defvar *user-base-id* 1000
  "The base integer user ID for connections.")

(defvar *servers* nil
  "A list of running WebSocket server instances.")

(defvar *main-start-port* 60000
  "The starting port for communication.")

(defvar *main-server* nil
  "The msl server instance.")

(defvar *admin-start-port* 60500
  "The starting port for admin access.")

(defvar *admin-server* nil
  "The admin server instance.")

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
  (websocket-driver:send connection (format-msl "VER" *system-version*)))

(defun echo-message (connection message)
  "Echo back MESSAGE to CONNECTION."
  (websocket-driver:send connection message))

(defun respond-expr (connection message)
  "Return the full MSL expression from MESSAGE."
  (let ((value (collect-expr message)))
    (websocket-driver:send connection value)))

(defun respond-value (connection message)
  "Return the default expression value from MESSAGE."
  (let ((value (collect-value message)))
    (websocket-driver:send connection value)))

(defun handle-close-connection (connection)
  (let ((message (format nil " ... ~A has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections*
          :do (websocket-driver:send con message))))

(defun start-server (server port)
  "Start server SERVER under port PORT."
  (clack:clackup server :port port))

(defun stop-server (server)
  "Stop server SERVER."
  (clack:stop server))

(defun main-server (env)
  "Handle requests to the msl server."
  (let ((server (websocket-driver:make-server env)))
    (websocket-driver:on :open server
                         (lambda () (handle-open-connection server)))
    (websocket-driver:on :message server
                         (lambda (message) (respond-expr server message)))
    (websocket-driver:on :close server
                         (lambda (&key _ __)
                           (declare (ignore _ __))
                           (handle-close-connection server)))
    (lambda (_)
      (declare (ignore _))
      (websocket-driver:start-connection server))))

(defun start-main-server ()
  "Start the msl WebSocket server."
  (let ((server (start-server #'main-server *main-start-port*)))
    (setf *main-server* server)
    server))

(defun stop-main-server ()
  "Stop the msl WebSocket server."
  (stop-server *main-server*)
  (setf *main-server* nil))

(defun admin-server (env)
  "Handle requests to the admin server."
  (let ((server (websocket-driver:make-server env)))
    (websocket-driver:on :open server
                         (lambda () (handle-open-connection server)))
    (websocket-driver:on :message server
                         (lambda (message) (respond-value server message)))
    (websocket-driver:on :close server
                         (lambda (&key _ __)
                           (declare (ignore _ __))
                           (handle-close-connection server)))
    (lambda (_)
      (declare (ignore _))
      (websocket-driver:start-connection server))))

(defun start-admin-server ()
  "Start the admin WebSocket server."
  (let ((server (start-server #'admin-server *admin-start-port*)))
    (setf *admin-server* server)
    server))

(defun stop-admin-server ()
  "Stop the admin WebSocket server."
  (stop-server *admin-server*)
  (setf *admin-server* nil))

(defun start-websocket-server (server &rest args)
  "Start the designated WebSocket server."
  (format t "Starting WebSocket server...~%")
  (let ((server (apply server args)))
    (push server *servers*)
    server))

(defun stop-websocket-servers ()
  "Stop all the WebSocket servers."
  (muffle-debugger)
  (format *error-output* "Aborting...~%")
  (loop :for server :in *servers* :do (clack:stop server))
  (setf *servers* nil)
  (uiop:quit))

(defun* (serve t) ()
  "The main entrypoint of the server."
  (start-websocket-server #'start-main-server)
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
