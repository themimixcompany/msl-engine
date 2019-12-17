;;;; websocket.lisp

(in-package #:engine)

(defvar *connections* (make-hash-table)
  "The table of connections, where the keys are server instances while the values are the user IDs.")

(defvar *html*
  (mof:slurp-file (mof:resolve-system-file "index.html" :engine))
  "The default HTML browser code.")

(defvar *user-base-id* 1000
  "The base integer user ID for connections.")

(defvar *msl-start-port* 60000
  "The starting port for (msl) communication.")

(defvar *admin-start-port* 60500
  "The starting port for admin access.")

(defun get-new-user-id ()
  "Return a new fresh user ID."
  (incf *user-base-id*))

(defun handle-open-connection (connection)
  "Add a new entry to the connections table, with the connection itself as the key."
  (setf (gethash connection *connections*)
        (format nil "user-~A" (get-new-user-id))))

(defun echo-message (connection message)
  "Echo back MESSAGE to CONNECTION."
  (websocket-driver:send connection message))

(defun handle-close-connection (connection)
  (let ((message (format nil " ... ~A has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections*
          :do (websocket-driver:send con message))))

(defun echo-server (env)
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

(defun start-server (server port)
  "Start SERVER using Clack."
  (clack:clackup server :port port))

(defun stop-server (server)
  "Stop SERVER."
  (clack:stop server))

(defvar *echo-server* nil "The echo server.")

(defun start-echo-server ()
  "Start the WebSocket echo server."
  (setf *echo-server* (start-server #'echo-server *msl-start-port*)))

(defun stop-echo-server ()
  "Stop the WebSocket echo server."
  (stop-server *echo-server*)
  (setf *echo-server* nil))

(defun start-websocket-server (&rest args)
  "Start the designated WebSocket server."
  (apply #'start-echo-server args))

(defun main ()
  (start-websocket-server)
  (handler-case (bt:join-thread (find-if (lambda (thread)
                                           (search "hunchentoot" (bt:thread-name thread)))
                                         (bt:all-threads)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     #+lispworks mp:process-interrupt
     () (progn
          (format *error-output* "Aborting.~&")
          (clack:stop *echo-server*)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
