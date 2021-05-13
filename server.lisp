;;;; server.lisp

(uiop:define-package #:msl-engine/server
  (:use #:cl
        #:websocket-driver
        #:msl-engine/specials
        #:msl-engine/config
        #:msl-engine/classes
        #:msl-engine/common
        #:msl-engine/parser
        #:msl-engine/logger
        #:msl-engine/reader
        #:msl-engine/bridge
        #:msl-engine/admin-writer
        #:msl-engine/json
        #:marie)
  (:export #:server))


(in-package #:hunchentoot)

(defmethod process-connection :around ((*acceptor* acceptor) (socket t))
  (handler-case (progn
                  ;;(format t "~A~%" (usocket:socket-stream socket))
                  ;;(force-output *standard-output*)
                  (with-mapped-conditions ()
                    (call-next-method)))
    (error (c)
      (values nil c))))


(in-package #:msl-engine/server)


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
  (when (∧ wire data)
    (send wire data)))

(def connection-headers (connection)
  "Return the header table from CONNECTION."
  (websocket-driver.ws.server::headers connection))

(def dump-headers (connection)
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
    ;;(debug-print (fmt "~A" (gethash "user-agent" table)))
    (post connection (admin-dispatch "(@VER)"))))

(defun echo-message (connection message)
  "Send back MESSAGE to CONNECTION."
  (send connection message))

(defun handle-close (connection)
  "Process connection CONNECTION when it closes."
  (let ((table (connection-headers connection)))
    (debug-print (fmt "Disconnection request received from ~A to ~A."
                      (gethash "origin" table)
                      (gethash "host" table)))
    ;;(dump-headers connection)
    (remhash connection *connections*)))


;;--------------------------------------------------------------------------------------------------
;; main handlers
;;--------------------------------------------------------------------------------------------------

(defvar *servers* nil
  "The list of active servers.")

(defv *listen-address* (config-value 'listen-address)
  "The address to listen to on this machine.")

(defun clack-start (server-name port)
  "Start the clack server SERVER under port PORT."
  (let* ((server :hunchentoot)
         (address *listen-address*)
         (port port)
         (value (clack:clackup server-name :server server :address address :port port :silent t)))
    (when value
      (debug-print (fmt "Web server is started." (string-capitalize (string* server))))
      (debug-print (fmt "Listening on ~A:~A." address port))
      value)))

(defun clack-stop (server)
  "Stop the clack server SERVER."
  (clack:stop server))

(defm def-runner (name port open-handler
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
             (λ (arg)
               (declare (ignore arg))
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

(def-runner "ADMIN" 60500
  (λ ()
    (handle-open server))
  (λ (message)
    (debug-print (fmt "Received on admin wire: ~S" message))
    (destructuring-bind (&optional expr js-data)
        (message-data message)
      (when expr
        (flet ((fn (val)
                 (make-return-data message val js-data)))
          (let ((value (fn (admin-dispatch expr))))
            (post *admin-wire* value)
            (debug-print (fmt "Sent on admin wire: ~S" value)))))))
  (λ (&key code reason)
    (declare (ignore code reason))
    (handle-close server))
  (λ (error)
    (debug-print (fmt "Got an error: ~A" error))))

(def-runner "MSL" 60000
  (λ ()
    (handle-open server))
  (λ (message)
    (debug-print (fmt "Received on MSL wire: ~S" message))
    (destructuring-bind (&optional expr js-data)
        (message-data message)
      (when expr
        (multiple-value-bind (expr value)
            (recall expr)
          (flet ((fn (val)
                   (make-return-data message val js-data)))
            (let* ((expr-value (fn expr))
                   (value-value (fn value)))
              (post *msl-wire* expr-value)
              (debug-print (fmt "Sent on MSL wire: ~S" expr-value))
              (post *admin-wire* value-value)
              (debug-print (fmt "Sent on admin wire: ~S" value-value))))))))
  (λ (&key code reason)
    (declare (ignore code reason))
    (handle-close server))
  (λ (error)
    (debug-print (fmt "Got an error: ~A" error))))

(defun start-servers ()
  "Start all the servers."
  (print-banner)
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

(def start-slynk-server ()
  "Start a slynk server."
  (let ((port (find-open-port))
        (*slynk-debug-p* nil))
    (when port
      (slynk:create-server :port port :dont-close t)
      (debug-print (fmt "Slynk open at ~A." port)))))

(def dump-threads ()
  "Print a list of running threads."
  (loop :for thread :in (bt:all-threads)
        :do (format t "~A~%" thread)))

(def dump-thread-names ()
  "Print a list of the names of the running threads."
  (loop :for thread :in (bt:all-threads)
        :do (format t "~A~%" (bt:thread-name thread))))

(def serve (&key slynk)
  "The main entrypoint of the server."
  (when *restore-log* (restore-log))
  (flet ((find-threads (query)
           (bt:join-thread (find-if (λ (thread)
                                      (search query (bt:thread-name thread)))
                                    (bt:all-threads)))))
    (handler-bind ((#+sbcl sb-sys:interactive-interrupt
                    #+ccl ccl:interrupt-signal-condition
                    #+ecl ext:interactive-interrupt
                    #+clisp system::simple-interrupt-condition
                    #+allegro excl:interrupt-signal
                    #+lispworks mp:process-interrupt
                    (λ (c)
                      (declare (ignore c))
                      (stop-servers)))
                   (error
                     (λ (c)
                       (format t "Oops, an unknown error occured: ~A~%" c))))
      (when slynk (start-slynk-server))
      (start-servers)
      (find-threads "hunchentoot"))))
