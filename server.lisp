;;;; server.lisp

(uiop:define-package #:streams/server
  (:use #:cl
        #:websocket-driver
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
  "The MSL server instance.")

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
  "Process incoming connection CONNECTION."
  (let ((uid (get-new-user-id))
        (message (format-msl "VER" *system-version*)))
    (setf (gethash connection *connections*)
          (format nil "user-~A" uid))
    (send connection message)))

(defun respond-expr (connection message)
  "Return the full MSL expression from MESSAGE."
  (let ((value (collect-expr message)))
    (send connection value)))

(defun respond-value (connection message)
  "Return the default expression value from MESSAGE."
  (let ((value (collect-value message)))
    (send connection value)))

(defun handle-close-connection (connection)
  "Process connection CONNECTION when it closes."
  (let ((message (format nil " ... ~A has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections*
          :do (send con message))))

(defun start-server (server port)
  "Start server SERVER under port PORT."
  (clack:clackup server :port port))

(defun stop-server (server)
  "Stop server SERVER."
  (clack:stop server))

(defun main-server (env)
  "Handle requests to the MSL server."
  (let ((server (make-server env)))
    (on :open server (lambda () (handle-open-connection server)))
    ;; eval
    ;; review evaluation rules
    ;; respond to both main and admin ports
    (on :message server (lambda (message) (respond-expr server message)))
    (on :close server (lambda (&key _ __)
                        (declare (ignore _ __))
                        (handle-close-connection server)))
    (lambda (_)
      (declare (ignore _))
      (start-connection server))))

(defun start-main-server ()
  "Start the MSL WebSocket server."
  (let ((server (start-server #'main-server *main-start-port*)))
    (setf *main-server* server)
    server))

(defun stop-main-server ()
  "Stop the MSL WebSocket server."
  (stop-server *main-server*)
  (setf *main-server* nil))

(defun admin-server (env)
  "Handle requests to the admin server."
  (let ((server (make-server env)))
    (on :open server (lambda () (handle-open-connection server)))
    (on :message server (lambda (message) (respond-value server message)))
    (on :close server (lambda (&key _ __)
                        (declare (ignore _ __))
                        (handle-close-connection server)))
    (lambda (_)
      (declare (ignore _))
      (start-connection server))))

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
  (with-muffled-debugger
    (format *error-output* "Aborting...~%")
    (loop :for server :in *servers* :do (clack:stop server))
    (setf *servers* nil)
    (uiop:quit)))

(defun* (serve t) ()
  "The main entrypoint of the server."
  (format t "streams v~A~%" *system-version*)
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
