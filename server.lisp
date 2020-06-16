;;;; server.lisp

(uiop:define-package #:streams/server
  (:use #:cl
        #:websocket-driver
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        #:streams/admin-dispatcher
        #:marie)
  (:export #:server))


(in-package #:hunchentoot)

(defmethod process-connection :around ((*acceptor* acceptor) (socket t))
  (handler-case (progn
                  (format t "~A~%" (usocket:socket-stream socket))
                  (force-output *standard-output*)
                  (with-mapped-conditions ()
                    (call-next-method)))
    (error (c)
      (values nil c))))


(in-package #:streams/server)


;;--------------------------------------------------------------------------------------------------
;; general helpers
;;--------------------------------------------------------------------------------------------------

(defvar *connections* (make-hash-table)
  "The table of connections, where the keys are server instances while the values are the connection IDs.")

(defvar *base-connection-id* 1000
  "The base integer connection ID for connections.")

(defun new-connection-id ()
  "Return a new fresh connection ID."
  (incf *base-connection-id*)
  *base-connection-id*)


;;--------------------------------------------------------------------------------------------------
;; generic handlers
;;--------------------------------------------------------------------------------------------------

(defun post (wire data)
  "Send DATA to WIRE if it contains valid information."
  (when (and wire data)
    (send wire data)))

(defun* connection-headers (connection)
  "Return the header table from CONNECTION."
  (websocket-driver.ws.server::headers connection))

(defun* dump-headers (connection)
  "Print the headers information from CONNECTION."
  (dump-table (connection-headers connection)))

(defun dump-thread-count ()
  "Print the current thread count information."
  (debug-print (fmt "Thread count: ~A" (length (bt:all-threads)))))

(defun handle-open (connection)
  "Process incoming connection CONNECTION."
  (let ((uid (new-connection-id))
        (table (connection-headers connection)))
    (setf (gethash connection *connections*)
          (fmt "~A" uid))
    (debug-print (fmt "Connection request received from ~A to ~A."
                      (gethash "origin" table)
                      (gethash "host" table)))
    (debug-print (fmt "~A" (gethash "user-agent" table)))
    ;;(dump-thread-count)
    (post connection (admin-dispatch "(@VER)"))))

(defun echo-message (connection message)
  "Send back MESSAGE to CONNECTION."
  (send connection message))

(defun handle-close (connection)
  "Process connection CONNECTION when it closes."
  (handler-bind ((usocket:unknown-error
                   (lambda (c)
                     (format t "Caught condition ~A" c))))
    (let ((table (connection-headers connection)))
      (debug-print (fmt "Disconnection request received from ~A to ~A."
                        (gethash "origin" table)
                        (gethash "host" table)))
      ;;(dump-thread-count)
      (remhash connection *connections*))))


;;--------------------------------------------------------------------------------------------------
;; main handlers
;;--------------------------------------------------------------------------------------------------

(defvar *servers* nil
  "The list of active servers.")

(defun clack-start (server-name port)
  "Start the clack server SERVER under port PORT."
  (let* ((server :hunchentoot)
         (address "127.0.0.1")
         (port port)
         (debug nil)
         (value (clack:clackup server-name :server server :address address :port port :silent t)))
    (when value
      (debug-print (fmt "~A server is started." (string-capitalize (string* server))))
      (debug-print (fmt "Listening on ~A:~A." address port))
      value)))

(defun clack-stop (server)
  "Stop the clack server SERVER."
  (clack:stop server))

(defmacro* define-runners (name form port open-handler
                                message-handler close-handler
                                error-handler)
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
             (websocket-driver:on :error server ,error-handler)
             (lambda (arg)
               (declare (ignore arg))
               ;; (handler-case (websocket-driver:start-connection server)
               ;;   (error (c) (format t "Caught error: ~A" c)))
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
    (debug-print (fmt "Received on admin wire: ~A" message))
    (let ((value (admin-dispatch message)))
      (post *admin-wire* value)
      (debug-print (fmt "Sent on admin wire: ~A" value)))
    ;;(dump-thread-count)
    )
  (lambda (&key code reason)
    (declare (ignore code reason))
    (handle-close server))
  (lambda (error)
    (debug-print (fmt "Got an error: ~A" error))))

(define-runners "MSL" 'msl 60000
  (lambda ()
    (handle-open server))
  (lambda (message)
    (debug-print (fmt "Received on MSL wire: ~A" message))
    (dispatch message)
    (let ((recall-expr-value (recall-expr message))
          (recall-value-value (recall-value message)))
      (post *msl-wire* recall-expr-value)
      (debug-print (fmt "Sent on MSL wire: ~A" recall-expr-value))
      (post *admin-wire* recall-value-value)
      (debug-print (fmt "Sent on admin wire: ~A" recall-value-value)))
    ;;(dump-thread-count)
    )
  (lambda (&key code reason)
    (declare (ignore code reason))
    (handle-close server))
  (lambda (error)
    (debug-print (fmt "Got an error: ~A" error))))

(defun start-servers ()
  "Start all the servers."
  (debug-print "Loading servers...")
  (start-admin-server)
  (start-msl-server))

(defun stop-servers ()
  "Stop all the servers."
  (debug-print "Stopping servers...")
  (stop-msl-server)
  (stop-admin-server)
  (uiop:quit))

(defun find-open-port ()
  "Return an open for slynk."
  (find-port:find-port :min 40000 :max 50000))

(defun* start-slynk-server ()
  "Start a slynk server."
  (let ((port (find-open-port))
        (*slynk-debug-p* nil))
    (when port
      (slynk:create-server :port port :dont-close t)
      (debug-print (fmt "Slynk open at ~A." port)))))

(defun* dump-threads ()
  "Print a list of running threads."
  (loop :for thread :in (bt:all-threads)
        :do (format t "~A~%" thread)))

(defun* dump-thread-names ()
  "Print a list of the names of the running threads."
  (loop :for thread :in (bt:all-threads)
        :do (format t "~A~%" (bt:thread-name thread))))

(defun* serve (&key slynk)
  "The main entrypoint of the server."
  (flet ((find-threads (query)
           (bt:join-thread (find-if #'(lambda (thread)
                                        (search query (bt:thread-name thread)))
                                    (bt:all-threads)))))
    (handler-bind ((#+sbcl sb-sys:interactive-interrupt
                    #+ccl ccl:interrupt-signal-condition
                    #+clisp system::simple-interrupt-condition
                    #+ecl ext:interactive-interrupt
                    #+allegro excl:interrupt-signal
                    #+lispworks mp:process-interrupt
                    #'(lambda (c)
                        (declare (ignore c))
                        (stop-servers)))
                   (error
                     #'(lambda (c)
                         (format t "Oops, an unknown error occured: ~A~%" c))))
      (when slynk (start-slynk-server))
      (start-servers)
      (find-threads "hunchentoot"))))
